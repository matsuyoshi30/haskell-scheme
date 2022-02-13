module Main where
import Control.Monad
import Control.Monad.Except
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

data LispError = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where show = showErr

showErr :: LispError -> String
showErr (BadSpecialForm message form) = message ++ ": " ++ show form
showErr (UnboundVar message varname) = message ++ ": " ++ varname
showErr (NotFunction message func) = message ++ ": " ++ show func
showErr (NumArgs expected found) = "Expected " ++ show expected ++ "args; found values " ++ unwordList found
showErr (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showErr (Parser pe) = "Parse error at " ++ show pe

type ThrowsError = Either LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
             ("-", numericBinOp (-)),
             ("*", numericBinOp (*)),
             ("/", numericBinOp div),
             ("mod", numericBinOp mod),
             ("quotient", numericBinOp quot),
             ("remainder", numericBinOp rem),
             ("boolean?", unaryOp boolp),
             ("symbol?", unaryOp symbolp),
             ("string?", unaryOp stringp),
             ("number?", unaryOp numberp),
             ("char?", unaryOp charp),
             ("list?", unaryOp listp),
             ("vector?", unaryOp vectorp),
             ("symbol->string", symbolToStr),
             ("string->symbol", stringToSym),
             ("=", numBoolBinOp (==)),
             ("/=", numBoolBinOp (/=)),
             (">", numBoolBinOp (>)),
             ("<", numBoolBinOp (<)),
             (">=", numBoolBinOp (>=)),
             ("<=", numBoolBinOp (<=)),
             ("&&", boolBoolBinOp (&&)),
             ("||", boolBoolBinOp (||))]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackedNum params >>= return . Number . foldl1 op

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinOp = boolBinOp unpackedNum
boolBoolBinOp = boolBinOp unpackedBool

unpackedNum :: LispVal -> ThrowsError Integer
unpackedNum (Number n) = return n
unpackedNum notNum = throwError $ TypeMismatch "number" notNum

unpackedBool :: LispVal -> ThrowsError Bool
unpackedBool (Bool b) = return b
unpackedBool notBool = throwError $ TypeMismatch "boolean" notBool

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _ = Bool False

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _ = Bool False

charp :: LispVal -> LispVal
charp (Character _) = Bool True
charp _ = Bool False

listp :: LispVal -> LispVal
listp (List _) = Bool True
listp _ = Bool False

vectorp :: LispVal -> LispVal
vectorp (Vector _) = Bool True
vectorp _ = Bool False

symbolToStr :: [LispVal] -> ThrowsError LispVal
symbolToStr [(Atom atom)] = return $ String atom
symbolToStr notSym = throwError $ TypeMismatch "symbol" (List notSym)

stringToSym :: [LispVal] -> ThrowsError LispVal
stringToSym [(String str)] = return $ Atom str
stringToSym notStr = throwError $ TypeMismatch "string" (List notStr)

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

trapError action = catchError action (return . show)

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
