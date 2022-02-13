module Main where
import Control.Monad
import Data.Array
import Data.Ratio
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | Ratio Rational
  | String String
  | Bool Bool
  | Character Char
  | Vector (Array Int LispVal)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ showVal tail ++ ")"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character ch) = "'" ++ show ch ++ "'"
showVal (Vector arr) = "#(" ++ unwordList (elems arr) ++ ")"

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal

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

parseCharacter :: Parser LispVal
parseCharacter = do
  ch <- try $ do
    _ <- char '#'
    _ <- char '\\'
    c <- string "space" <|> string "newline" <|> do
      x <- anyChar
      _ <- notFollowedBy alphaNum
      return [x]
    return c
  return $ Character $ case ch of
    "space" -> ' '
    "newline" -> '\n'
    _ -> ch !! 0

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

parseDec :: Parser LispVal
parseDec = liftM (Number . read) $ many1 digit

parseHex :: Parser LispVal
parseHex = do
  n <- try $ do
    _ <- char '#'
    _ <- char 'x'
    h <- many1 hexDigit
    return $ fst ((readHex h) !! 0)
  return $ Number n

parseOct :: Parser LispVal
parseOct = do
  n <- try $ do
    _ <- char '#'
    _ <- char 'o'
    o <- many1 octDigit
    return $ fst ((readOct o) !! 0)
  return $ Number n

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

parseFloat :: Parser LispVal
parseFloat = do
  f <- try $ do
    ip <- many1 digit
    _ <- char '.'
    dp <- many1 digit
    return $ fst ((readFloat (ip ++ "." ++ dp)) !! 0)
  return $ Float f

parseRatio :: Parser LispVal
parseRatio = do
  r <- try $ do
    n <- many1 digit
    _ <- char '/'
    d <- many1 digit
    return $ (read n) % (read d)
  return $ Ratio r

-- TODO: parseComplex

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
  _ <- char ','
  _ <- char '@'
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do
  es <- try $ do
    _ <- char '#'
    _ <- char '('
    vals <- sepBy parseExpr spaces
    _ <- char ')'
    return vals
  return $ Vector (listArray (0, (length es - 1)) es)

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseCharacter <|> parseRatio <|> parseFloat <|> parseNumber <|> parseAtom <|> parseVector <|> parseBool <|> parseQuoted  <|> parseQuasiQuoted <|> parseUnQuoteSplicing <|> parseUnQuote <|> do
  _ <- char '('
  x <- try parseList <|> try parseDottedList
  _ <- char ')'
  return x

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinOp (+)),
             ("-", numericBinOp (-)),
             ("*", numericBinOp (*)),
             ("/", numericBinOp div),
             ("mod", numericBinOp mod),
             ("quotient", numericBinOp quot),
             ("remainder", numericBinOp rem),
             ("boolean?", unaryOp boolp)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackedNum params

unpackedNum :: LispVal -> Integer
unpackedNum (Number n) = n
unpackedNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _ = Bool False

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
