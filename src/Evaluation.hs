module Evaluation where
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import MyParser
import MyError 

eval :: LispVal -> ThrowError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List ((Atom func) : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "bad special form" badForm

apply :: String -> [LispVal] -> ThrowError LispVal
apply func args = maybe (throwError $ NotFunction "bad primitive function" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op oneV@[_] = throwError $ NumArgs 2 oneV
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
                            if null parsed
                            then throwError $ TypeMismatch "number" $ String s
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) =unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

