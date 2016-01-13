module Main where
import SchemeParser 
import System.Environment
import Control.Monad.Error
import Text.ParserCombinators.Parsec

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
                                     " args; found values " ++ 
                                     (unwords . map showVal) found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at" ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError
trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

instance Show LispVal where show = showVal

{-- Custom string representation --}
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char c) =  "'\\\"" ++ [c,'\'']
showVal (List contents) = "(" ++ (unwords . map showVal) contents ++ ")"
showVal (DottedList head tail) = "(" ++ (unwords . map showVal) head 
                                     ++ " . " ++ showVal tail ++ ")"

{-- Our current evaluator, merely just prints out the expr --}
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (parseExpr <* eof) "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive\
                                                   \ function args" func) 
                        ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]
--              ("number?", isNumber),
--              ("string?", isString),
--              ("symbol?", isSymbol),
--              ("symbol->string", symToStr),
--              ("string->symbol", strToSym)]

isNumber :: [LispVal] -> LispVal
isNumber [(Number _)] = Bool True
isNumber _ = Bool False

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _ = Bool False

symToStr :: [LispVal] -> LispVal
symToStr [Atom s] = String s
symToStr _ = String "YOU SUCK"

strToSym :: [LispVal] -> LispVal
strToSym [String s] = Atom s
strToSym _ = Atom "YOU SUCK2"

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> 
                ThrowsError LispVal

numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                               then throwError $ TypeMismatch "number" $ String n
                               else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

{- unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then 0
                                else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n -}

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled
-- main = getArgs >>= putStrLn . show . eval . readExpr . head
-- main = getArgs >>= putStrLn . showVal . readExpr . head

