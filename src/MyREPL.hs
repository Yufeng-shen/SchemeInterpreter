module MyREPL where
import System.IO
import Evaluation
import MyError
import Control.Monad
import MyParser
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error

readExpr :: String -> ThrowError LispVal
readExpr input= case parse (spaces >> parseExpr) "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
