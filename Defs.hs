module Defs where

import Control.Monad.Except
import Data.IORef
import Text.ParserCombinators.Parsec(ParseError)
import System.IO

data LispVal =  Atom String
              | List [LispVal] -- proper list
              | DottedList [LispVal] LispVal -- (a b . c), improper list, last elem 
                                             -- different field
              | Number Integer
              | String String
              | Bool Bool 
              | Char Char
              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | IOFunc ([LispVal] -> IOThrowsError LispVal)
              | Func { params :: [String], vararg :: (Maybe String),
                       body :: [LispVal], closure :: Env }
              | Port Handle

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

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
showVal (IOFunc _) = "<IO primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++ 
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"

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
showError (Parser parseErr) = "Parse error at " ++ show parseErr

nullEnv :: IO Env
nullEnv = newIORef []

