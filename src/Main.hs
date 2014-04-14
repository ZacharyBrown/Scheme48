module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)

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
    
    
    
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
    
parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber
    
              
parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x
    
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
parseNumber = liftM (Number . read) $ many1 digit




