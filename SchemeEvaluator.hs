module Main where
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

import Defs
import Primitives
import SchemeParser 


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (List [Atom "load", String filename]) =
    load filename >>= fmap last . traverse (eval env)

eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =  -- Can return different types -_-
    do result <- eval env pred
       case result of
            Bool False -> eval env alt
            otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = 
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = 
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = 
    makeVarArgs varargs env [] body

-- Just have it return its string representation for now
eval env (List (Atom "let" : List bindings : body)) =  
    resolveLet env bindings body False -- Abstract away the bool, has no abstract meaning

eval env (List (Atom "let*" : List bindings : body)) =  
    resolveLet env bindings body True
-- We'll want to create a sub_env, and run the body in its context

eval env (List (function : args)) = eval env function    >>= \func ->
                                    traverse (eval env) args >>= apply func

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

resolveLet :: Env -> [LispVal] -> [LispVal] -> Bool -> IOThrowsError LispVal
resolveLet env bindings body inc = 
    if all isBinding bindings then
        if inc then foldM bindVar env (fmap convBinding bindings) >>= evalBody
        else fmap (reverse . zip atoms) evaledRHSs >>= liftIO . bindVars env >>= evalBody
    else throwError $ BadSpecialForm "Incorrect let binding format" $ getBadBinding bindings
    where getBadBinding = fromJust . find (not . isBinding) 
          evalBody env = fmap last $ traverse (eval env) body
          convBinding (List [atom, expr]) = (showVal atom, expr)
          atoms = fst . unzip $ fmap convBinding $ bindings
          evaledRHSs = traverse (eval env) . snd . unzip $ fmap convBinding $ bindings
          isBinding (List [Atom _, expr]) = True
          isBinding _ = False

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= 
             bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = fmap last $ traverse (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ 
                                                      remainingArgs)]
              Nothing -> return env
apply val _ = throwError $ NotFunction "Not a function, dummy" $ show val

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) .
                                                    lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do valueRef <- newIORef value
                         env <- readIORef envRef
                         writeIORef envRef ((var, valueRef) : env)
                         return value

bindVar :: Env -> (String, LispVal) -> IOThrowsError Env
bindVar envRef (var, val) = eval envRef val           >>= \eVal ->
                            liftIO $ readIORef envRef >>= \env  ->
                                     newIORef eVal    >>= \ref  ->
                                     newIORef ((var,ref) : env)

-- Creates a new IORef consisting of the existing env, plus new bound vars
-- Does NOT modify existing ref
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = fmap (++ env) (traverse addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = {-nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
                    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)-}
                    nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives 
                                ++ map (makeFunc PrimitiveFunc) primitives)
                    where makeFunc constructor (var, func) = (var, constructor func)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = 
    if all isAtom params
    then return $ Func (fmap showVal params) varargs body env 
    else throwError $ TypeMismatch "atom" $ fromJust . find (not . isAtom) $ params
    where isAtom (Atom _) = True
          isAtom _ = False
          

makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal 

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = let trapError action = action `catchError` (return . ("Error: " ++) . show) 
                         extractValue (Right val) = val in
                     runExceptT (trapError action) >>= return . extractValue

{-- Our current evaluator, merely just prints out the expr --}
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse (parser <* eof) "lisp" input of
                                Left err -> throwError $ Parser err
                                Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow $ endBy parseExpr spaces


evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap (("=> " ++) . show) $ 
                          (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

-- Maybe use monadplus to ad infinitum hold until something of value is passed
-- into stdin
until_ :: (String -> Bool) -> IO String -> (String -> IO ()) -> String -> IO ()
until_ pred prompt action prefix = while (all isSpace) prompt >>= \result ->
                            if pred result
                            then return () else 
                            let newResult = prefix ++ (' ' : result) in
                            if matchParens newResult
                            then action newResult >> until_ pred (readPrompt "> ") action ""
                            else until_ pred (readPrompt "... ") action newResult
                        where matchParens xs = (length . filter (== '(') $ xs) <= 
                                               (length . filter (== ')') $ xs) 
                              while pred action = action >>= \expr ->
                                                  if pred expr then while pred action
                                                  else return expr

readPrompt :: String -> IO String
readPrompt prompt = let flushStr str = putStr str >> hFlush stdout in
                    flushStr prompt >> getLine

runRepl :: IO ()
runRepl = primitiveBindings >>= flip (until_ (== "quit") (readPrompt "> ")) "" . 
          evalAndPrint

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"

