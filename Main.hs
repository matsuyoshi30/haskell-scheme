module Main where
import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving Show

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = do
  _ <- char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _ -> x

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ escapeChar <|> (noneOf "\\\"")
  _ <- char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

-- parseDec
parseDec :: Parser LispVal
parseDec = liftM (Number . read) $ many1 digit

-- parseHex
parseHex :: Parser LispVal
parseHex = do
  n <- try $ do
    _ <- char '#'
    _ <- char 'x'
    h <- many1 hexDigit
    return $ fst ((readHex h) !! 0)
  return $ Number n

-- parseOct
parseOct :: Parser LispVal
parseOct = do
  n <- try $ do
    _ <- char '#'
    _ <- char 'o'
    o <- many1 octDigit
    return $ fst ((readOct o) !! 0)
  return $ Number n

-- parseBin
parseBin :: Parser LispVal
parseBin = do
  n <- try $ do
    _ <- char '#'
    _ <- char 'b'
    b <- many1 (oneOf "01")
    return $ bin2dig b
  return $ Number n

bin2dig :: String -> Integer
bin2dig = bin2dig' 0
bin2dig' :: Integer -> String -> Integer
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1)
  in bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber = parseDec <|> parseHex <|> parseOct <|> parseBin

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseNumber <|> parseAtom <|> parseBool

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
