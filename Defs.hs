module Defs where

import Text.ParserCombinators.Parsec(ParseError)
import Data.IORef

data LispVal =  Atom String
              | List [LispVal] -- proper list
              | DottedList [LispVal] LispVal -- (a b . c), improper list, last elem 
                                             -- different field
              | Number Integer
              | String String
              | Bool Bool 
              | Char Char
              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | Func { params :: [String], vararg :: (Maybe String),
                       body :: [LispVal], closure :: Env }
              | Let { locs :: [(String, LispVal)], body :: [LispVal] }

type ThrowsError = Either LispError
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type Env = IORef [(String, IORef LispVal)]

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++ 
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
-- For debug/dev purposes only, in prod 'let' expr will always be evaluated
-- to one of the other forms
showVal (Let {locs = bindings, body = body}) = 
    "Let ( " ++ (concatMap (\x -> "["++ x ++ "] ") $ fst $ unzip bindings) ++ ")"

instance Show LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
                                     " args; found values [" ++ 
                                     (unwords . map showVal) found ++ "]"
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at" ++ show parseErr

nullEnv :: IO Env
nullEnv = newIORef []

