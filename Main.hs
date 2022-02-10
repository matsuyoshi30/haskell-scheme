module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit -- many1 digit が Parser モナドを返すので、 liftM で関数 Number . read を持ち上げてモナド内の値に適用する

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseAtom <|> parseNumber

readExpr :: String -> String
readExpr input =
  case parse (spaces >> parseExpr) "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
