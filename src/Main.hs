module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex, readFloat)
import Data.Ratio
import Data.Complex
import Data.Array
import Data.List (intercalate)
import Control.Monad.Error


main :: IO()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

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

-- ERROR STUFF --

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default err) = err
instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An unknown error has occurred"
    strMsg = Default
    
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- Left catch is left undefined, as it represents a programming error


-- END ERROR STUFF --

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val --"Found value: " ++ show val
    
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
binaryStr2Int s = sum $ map (\(n,i) -> i*(2^n)) $ zip [0..] $ map convert (reverse s)
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






-- EVALUATION --


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, consequent, alternate]) = 
    do 
        result <- eval predicate
        case result of 
          Bool False -> eval alternate
          Bool True -> eval consequent
          _ -> throwError $ TypeMismatch "non boolean in \'if\' construct" predicate
eval (List (Atom func : args)) = mapM eval args >>= apply func 
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv)
              ]
              

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _   []      = throwError $ NumArgs 2 []
numericBinop _   val@[_] = throwError $ NumArgs 2 val
numericBinop oper params = mapM unpackNum params >>= return . Number . foldl1 oper

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""


car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker oper args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do 
                                    left <- unpacker $ args !! 0
                                    right <- unpacker $ args !! 1
                                    return $ Bool $ left `oper` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notString = throwError $ TypeMismatch "string" notString

