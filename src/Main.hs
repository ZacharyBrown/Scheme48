module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex)

main :: IO()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
    
--instance Show LispVal where
--    show val = case val of
--        (Atom a) -> a
----        (List xs) -> unwords . map show xs
----        (DottedList init last) -> init ++ "." ++ show last
--        (String s) -> s
--        (Number n) -> show n
        
    
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " -- ++ show val
    
parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber
        <|> parseBool
    
              
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ escapedChars <|> (noneOf "\"\\")
    _ <- char '"'
    return $ String x
    
escapedChars :: Parser Char
escapedChars = do
    _ <- char '\\'
    c <- oneOf ['\\', '"', 'n', 'r', 't']
    return $ case c of 
        '\\' -> c
        '"'  -> c
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'
    
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    (char 't' >> return (Bool True))
    <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseDefaultDec <|> parseRadixDelimitedNum

-- Parse 'normal' numbers
parseDefaultDec :: Parser LispVal
parseDefaultDec = many1 digit >>= return . Number . read 

-- Parse numbers beginning with '#'
parseRadixDelimitedNum :: Parser LispVal
parseRadixDelimitedNum =
    char '#'
    >> (parseDec)

-- Parse decimal numbers: beginning with "#d"
parseDec :: Parser LispVal
parseDec = do
    _ <- char 'd'
    num <- many1 digit
    (return . Number . read ) num

-- Parse binary numbers: beginning with "#b"

-- binary parser helper
-- needs to sum powers of 2 for each position in string that has a '1'
binaryStr2Int :: String -> Integer
binaryStr2Int s = 0

-- Parse octal numbers: beginning with "#o"
parseOctal :: Parser LispVal
parseOctal = do
    _ <- char 'o'
    num <- many $ oneOf "01234567"
    (return . Number . (readReadSWith readOct)) num

-- Parse hexidecimal numbers: beginning with "#x"
parseHex :: Parser LispVal
parseHex = do
    _ <- char 'x'
    num <- many $ oneOf "0123456789abcdefABCDEF"
    (return . Number . (readReadSWith readHex)) num

-- Octal and Hex parser helper
-- Both readOct and readHex return this ReadS type (first arg)
-- So we need to unpack it into just the value we want...
-- Assumes the first parse in the list from ReadS is good
readReadSWith :: (String -> [(a, String)]) -> String -> a
readReadSWith funct str = fst $ (funct str) !! 0

