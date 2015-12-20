module Main where
import SchemeParser 
import System.Environment
import Text.ParserCombinators.Parsec (eof, parse)

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
readExpr :: String -> String
readExpr input = case parse (parseExpr <* eof) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)


