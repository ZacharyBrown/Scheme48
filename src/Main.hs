module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex, readFloat)
import Data.Ratio
import Data.Complex
import Data.Array
import Data.List (intercalate)


main :: IO()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | Float Double
              | Complex (Complex Double)
              | Ratio Rational
              | String String
              | Character Char
              | Bool Bool
              | Vector (Array Int LispVal)
    
instance Show LispVal where show = showVal
        
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List vals) = "(" ++ unwordsList vals ++ ")"
showVal (DottedList initList lastItem) = "(" ++ unwordsList initList ++ " . " ++ showVal lastItem ++ ")"
showVal (Number num) = show num
showVal (Float num) = show num ++ "f"
showVal (Complex complex) = (show (realPart complex)) ++ " + " ++ (show (imagPart complex)) ++ "i"
showVal (Ratio ratio) = show ratio
showVal (String str) = "\"" ++ str ++ "\""
showVal (Character c) = show c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f" 
showVal (Vector arr) = "[" ++ showArray arr ++ "]"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showArray :: Array Int LispVal -> String
showArray arr = intercalate ", " $ map showVal $ elems arr 
    
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
    
parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> try parseRatio
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try (do 
                    _ <- string "#("
                    x <- parseVector
                    _ <- char ')'
                    return x)
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> do 
            _ <- char '('
            x <- (try parseList <|> parseDottedList)
            _ <- char ')'
            return x
            
        
              
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
    -- This is actually exhaustive because the list is right above... sorry compiler
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
    
parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- string "#\\"
    s <- many1 letter
    return $ Character $ case s of
        "space" -> ' '
        "newline" -> '\n'
        [_] -> s !! 0

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    (char 't' >> return (Bool True))
    <|> (char 'f' >> return (Bool False))

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    dot <- char '.'
    y <- many1 digit
    (return . Float . (readReadSWith readFloat)) (x ++ [dot] ++ y)
    
parseRatio :: Parser LispVal
parseRatio = do 
    numer <- many1 digit
    _ <- char '/'
    denom <- many1 digit
    -- '%' is a constructor for the Rational Type
    return $ Ratio ((read numer) % (read denom))
    
parseComplex :: Parser LispVal
parseComplex = do 
            real <- fmap toDouble (try parseFloat <|> parseDefaultDec)
            _ <- char '+'
            img <- fmap toDouble (try parseFloat <|> parseDefaultDec)
            _ <- char 'i'
            -- ":+" creates a Complex Type
            return $ Complex (real :+ img)
        where toDouble (Float x) = x
              toDouble (Number x) = fromIntegral x

parseNumber :: Parser LispVal
parseNumber = parseDefaultDec <|> parseRadixDelimitedNum

-- Parse 'normal' numbers
parseDefaultDec :: Parser LispVal
parseDefaultDec = many1 digit >>= return . Number . read 

-- Parse numbers beginning with '#'
parseRadixDelimitedNum :: Parser LispVal
parseRadixDelimitedNum =
    char '#'
    >> (parseDec <|> parseBinary <|> parseOctal <|> parseHex)

-- Parse decimal numbers: beginning with "#d"
parseDec :: Parser LispVal
parseDec = do
    _ <- char 'd'
    num <- many1 digit
    (return . Number . read ) num

-- Parse binary numbers: beginning with "#b"
parseBinary :: Parser LispVal
parseBinary = do
    _ <- char 'b'
    num <- many $ oneOf "01"
    (return . Number . binaryStr2Int) num

-- binary parser helper
-- needs to sum powers of 2 for each position in string that has a '1'
binaryStr2Int :: String -> Integer
binaryStr2Int s = sum $ map (\(i,n) -> i*(2^n)) $ zip [0..] $ map convert (reverse s)
        where convert '0' = 0
              convert '1' = 1

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


parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= return . List

parseDottedList :: Parser LispVal
parseDottedList = do
    headList <- endBy parseExpr spaces
    tailItem <- char '.' >> spaces >> parseExpr
    return $ DottedList headList tailItem

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
    
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    _ <- char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]
     
parseUnQuote :: Parser LispVal
parseUnQuote = do
    _ <- char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do 
    arrayValues <- sepBy parseExpr spaces
    -- listArray creates an Array from a tuple of bounds and a list of values
    return $ Vector (listArray (0, (length arrayValues) - 1) arrayValues)
