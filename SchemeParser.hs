module SchemeParser(LispVal(..), parseExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Debug.Trace
import Numeric (readHex, readOct)
import Data.Char (toLower, toUpper)

data LispVal =  Atom String
              | List [LispVal] -- proper list
              | DottedList [LispVal] LispVal -- (a b . c), improper list, last elem as
                                             -- different field
              | Number Integer
              | String String
              | Bool Bool 
              | Char Char

symbol :: Parser Char
symbol = oneOf "!$%&*+-./:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


{-- Character parsing, including special named chars --}
parseChar :: Parser LispVal
parseChar = string "#\\" >> (try parseCharName <|> anyChar) >>= return . Char
               
parseCharName :: Parser Char
parseCharName = do s <- try (string_insen "space" <|> string_insen "newline")
                   return $ case s of 
                                "space" -> ' '
                                "newline" -> '\n'      

char_insen :: Char -> Parser Char
char_insen c = (char $ toUpper c) <|> (char $ toLower c)

string_insen :: String -> Parser String
string_insen [] = return []
string_insen (c:cs) = char_insen c >> string_insen cs >> return (c:cs)

{-- General string parsing --}
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (parseEscape <|> noneOf ['\"'])
    char '"'
    return $ String x

parseEscape :: Parser Char
parseEscape = do char '\\'
                 c <- oneOf ['"', '\\', 'n', 'r', 't']
                 return $ case c of
                              'n' -> '\n'
                              'r' -> '\r'
                              't' -> '\t'
                              otherwise -> c

{-- I guess atoms are like identifiers??? --}

-- (<|>) is a parser combinator that "short-circuits"; tries the first parser,
-- and if it fails, then tries the second one; the operator is def'd in Parsec
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom
--    return $ case atom of
--        "#t" -> Bool True
--        "#f" -> Bool False
--        _    -> Atom atom

parseBool :: Parser LispVal
parseBool = char '#' >> (char 't' <|> char 'f') >>= 
                \x -> return $ if x == 't' then Bool True else Bool False

{-- ALL NUMERIC PARSERS --}
parseNumber :: Parser LispVal
parseNumber = parseBasicNumber <|> parseRadix

-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = (Number . read) <$> many1 digit
-- parseNumber = do n <- many1 digit
--                  return $ (Number . read) n
parseBasicNumber :: Parser LispVal
parseBasicNumber = many1 digit >>= return . Number . read

parseRadix :: Parser LispVal
parseRadix = char '#' >> ( parseOct <|>
                           parseHex <|>
                           parseDecimal )

readWith f s = fst $ f s !! 0

parseDecimal :: Parser LispVal
parseDecimal = char 'd' >> parseBasicNumber

parseHex :: Parser LispVal
parseHex = do char 'x'
              x <- many1 $ oneOf "0123456789abcdefABCDEF"
              return . Number . (readWith readHex) $ x 

parseOct :: Parser LispVal
parseOct = do char 'o'
              o <- many1 $ oneOf "01234567"
              return . Number . (readWith readOct) $ o

{-- (Dotted) List parsing --}
parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

{-- Scheme symbols (NOT identifiers, but like Ruby symbols) --}
parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= \x -> return $ List [Atom "quote", x]

{-- Putting it all together for an expression parser --}

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseQuoted
        <|> parseString 
        <|> try parseBool -- is this cheating? Is this too easy or inelegant?
        <|> try parseNumber 
        <|> try parseChar
        <|> ( char '(' >> (try parseList <|> parseDottedList) >>= \x -> char ')' 
              >> return x )

