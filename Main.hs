{-# LANGUAGE ExistentialQuantification #-}
module Main where
import Control.Monad
import Control.Monad.Except
import Data.Array
import Data.IORef
import Data.Ratio
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

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
showVal (Character ch) = show ch
showVal (Vector arr) = "#(" ++ unwordList (elems arr) ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++
  unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

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
showErr (Default message) = message

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

parseNegativeNum :: Parser LispVal
parseNegativeNum = do
  nn <- try $ do
    _ <- char '-'
    n <- many1 digit
    return $ (-1) * (read n)
  return $ Number nn

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
parseExpr = parseString <|> parseCharacter <|> parseRatio <|> parseFloat <|> parseNegativeNum <|> parseNumber <|> parseAtom <|> parseVector <|> parseBool <|> parseQuoted  <|> parseQuasiQuoted <|> parseUnQuoteSplicing <|> parseUnQuote <|> do
  _ <- char '('
  x <- try parseList <|> try parseDottedList
  _ <- char ')'
  return x

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef val))
    (lookup var env)
  return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var val >> return val
    else liftIO $ do
           valRef <- newIORef val
           env <- readIORef envRef
           writeIORef envRef ((var, valRef) : env)
           return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, val) = do
          ref <- newIORef val
          return (var, ref)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Character _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", test, conseq, alt]) = do
  result <- eval env test
  case result of
    Bool True -> eval env conseq
    Bool False -> eval env alt
    other -> throwError $ TypeMismatch "boolean" other
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (func : args)) = do
  f <- eval env func
  argVals <- mapM (eval env) args
  apply f argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>=
         evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
        Nothing -> return env
apply (IOFunc func) args = func args

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
             ("string-length", stringLength),
             ("make-string", makeString),
             ("string-ref", stringRef),
             ("=", numBoolBinOp (==)),
             ("/=", numBoolBinOp (/=)),
             (">", numBoolBinOp (>)),
             ("<", numBoolBinOp (<)),
             (">=", numBoolBinOp (>=)),
             ("<=", numBoolBinOp (<=)),
             ("&&", boolBoolBinOp (&&)),
             ("||", boolBoolBinOp (||)),
             ("string=?", strBoolBinOp (==)),
             ("string>?", strBoolBinOp (>)),
             ("string<?", strBoolBinOp (<)),
             ("string>=?", strBoolBinOp (>=)),
             ("string<=?", strBoolBinOp (<=)),
             ("car", car),
             ("cdr", cdr),
             ("cons", cons),
             ("eq?", eqv),
             ("eqv?", eqv),
             ("equal?", equal)]

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
strBoolBinOp = boolBinOp unpackedStr

unpackedNum :: LispVal -> ThrowsError Integer
unpackedNum (Number n) = return n
unpackedNum notNum = throwError $ TypeMismatch "number" notNum

unpackedBool :: LispVal -> ThrowsError Bool
unpackedBool (Bool b) = return b
unpackedBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackedStr :: LispVal -> ThrowsError String
unpackedStr (String s) = return s
unpackedStr (Number n) = return $ show n
unpackedStr notStr = throwError $ TypeMismatch "string" notStr

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

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [(String str)] = return $ Number $ toInteger $ length str
stringLength [notString] = throwError $ TypeMismatch "string" notString
stringLength notStr = throwError $ TypeMismatch "string" (List notStr)

makeString :: [LispVal] -> ThrowsError LispVal
makeString [(Number n), (Character ch)] = return $ String $ replicate (fromInteger n) ch
makeString [(Number _), notCharacter] = throwError $ TypeMismatch "character" notCharacter
makeString [notNumber, _] = throwError $ TypeMismatch "number" notNumber
makeString badArgList = throwError $ NumArgs 2 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String str), (Number n)]
  | length str <= (fromIntegral n) = throwError $ Default "Out of bounds error"
  | otherwise = return $ Character $ str !! (fromIntegral n)
stringRef [notString, _] = throwError $ TypeMismatch "string" notString
stringRef badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x -- (a . b) = Dottedlist [a] b ---> b
cdr [DottedList (_:xs) x] = return $ DottedList xs x -- (a b . c) = DottedList [a, b] c ---> (b . c)
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x] -- (cons x ()) ---> (x)
cons [x, List xs] = return $ List $ x:xs -- (cons x '(xs1 xs2)) ---> (x xs1 xs2)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast -- (cons x ('(xs1 xs2) . xlast)) ---> (x xs1 xs2 . xlast)
cons [x1, x2] = return $ DottedList [x1] x2 -- (cons x1 x2) ---> (x1 . x2)
cons badArgList = throwError $ NumArgs 1 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
          Right (Bool val) -> val
          _ -> False
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all equalPair $ zip arg1 arg2)
  where equalPair (x1, x2) = case equal [x1, x2] of
          Right (Bool val) -> val
          _ -> False
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  equalResult <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackedNum, AnyUnpacker unpackedBool, AnyUnpacker unpackedStr]
  eqvResult <- eqv [arg1, arg2]
  return $ Bool $ (equalResult || let (Bool x) = eqvResult in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- TODO: cond

-- TODO: case

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
               ("open-input-file", makePort ReadMode),
               ("open-output-file", makePort WriteMode),
               ("close-input-port", closePort),
               ("close-output-port", closePort),
               ("read", readProc),
               ("write", writeProc),
               ("read-contents", readContents),
               ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename)  >>= liftThrows . readExprList

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

trapError action = catchError action (return . show)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ terminateCond prompt action = do
  result <- prompt
  if terminateCond result
    then return ()
    else action result >> until_ terminateCond prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "MINI Haskell-Scheme >>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args
