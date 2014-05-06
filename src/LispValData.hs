module LispValData where

import Data.Ratio
import Data.Complex
import Data.Array
import Data.List (intercalate)

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
