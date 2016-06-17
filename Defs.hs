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


type ThrowsError = Either LispError
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

