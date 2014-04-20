module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

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
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
    
              
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ escapedChars <|> (noneOf "\"\\")
    _ <- char '"'
    return $ String x
    
escapedChars :: Parser Char
escapedChars = do
    _ <- char '\\'
    c <- oneOf "\"\\nrt"
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
    return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read


