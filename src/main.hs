module Main where
import System.Environment
import Control.Monad
import MyParser


main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn expr
    putStrLn (readExpr expr)
