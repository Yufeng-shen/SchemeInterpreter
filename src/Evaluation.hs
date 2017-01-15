module Evaluation where
import Text.ParserCombinators.Parsec hiding (spaces)
 
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\"" 
showVal (Atom name) = name 
showVal (Number contents) = show contents 
showVal (Bool True) = "#t" 
showVal (Bool False) = "#f" 
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
