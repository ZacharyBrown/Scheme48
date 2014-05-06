module Main where

import System.Environment
import Control.Monad.Error

import LispErrorData
import Evaluation (eval)
import LispValParsing (readExpr)

main :: IO()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled


