module Main where
import System.Environment
import Control.Monad
import MyParser
import Evaluation

main :: IO ()
main = do
    (expr:_) <- getArgs
    show . readExpr expr
