module MyParser where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!@#$%^&*()_+=-/:<=>?~"

readExpr :: String -> String
readExpr input= case parse (spaces >> parseExpr) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: "  ++ showVal val

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\"" ++ " String"
showVal (Atom name) = name ++ " Atom"
showVal (Number contents) = show contents ++ " Number"
showVal (Bool True) = "#t" ++ " Bool True"
showVal (Bool False) = "#f" ++ " Bool False"

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber

spaces :: Parser ()
spaces = skipMany space

