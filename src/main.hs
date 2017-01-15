module Main where
import System.Environment
import Control.Monad
import MyParser
import Evaluation
import Text.ParserCombinators.Parsec hiding (spaces)
import MyError
import Control.Monad.Error

readExpr :: String -> ThrowError LispVal
readExpr input= case parse (spaces >> parseExpr) "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
--    (expr:_) <- getArgs
--    (print . eval . readExpr) expr
--main = getArgs >>= print . eval . readExpr . head
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
