module LispDatatypes where

import Data.Complex
import Data.Array
import Data.List (intercalate)

-- imports for Error
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

-- imports for Environment
import Data.IORef
import System.IO (Handle)

data LispVal = Atom String
              | Nil
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
              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | Func {params :: [String], vararg :: (Maybe String),
                       body :: [LispVal], closure :: Env}
              | IOFunc ([LispVal] -> IOThrowsError LispVal)
              | Port Handle
    
instance Show LispVal where show = showVal
        
showVal :: LispVal -> String
showVal (Atom name) = name
showVal Nil = "" 
showVal (List [] ) = ""
showVal (List (Nil:xs)) = showVal $ List xs
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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of 
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList xs = unwords . map showVal $ xs

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


-- Environment Stuff --

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var )
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting and unbound variable" var)
          (liftIO . (flip writeIORef value))
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindingz env = liftM (++ env) (mapM addBinding bindingz)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

-- END Environment Stuff --
