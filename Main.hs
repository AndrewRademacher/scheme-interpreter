
{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Vector                                    as V
import           Text.ParserCombinators.Parsec hiding (spaces)
import           System.Environment
import           Numeric
import           Control.Monad.Error
import           System.IO
import           Data.IORef

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (V.Vector LispVal)
             | Number Integer
             | Float Double
             | String String
             | Char Char
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params  :: [String]
                    , vararg  :: (Maybe String)
                    , body    :: [LispVal]
                    , closure :: Env
                    }

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

type ThrowsError = Either LispError

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

--  PARSE
symbol :: Parser Char
symbol = oneOf "1#$%&|*+-/:<=>?@^_~"

parseBool :: Parser LispVal
parseBool = try true <|> false
    where true  = string "#t" >> return (Bool True)
          false = string "#f" >> return (Bool False)

parseString :: Parser LispVal
parseString = do
        char '"'
        x <- many (escaped <|> noneOf "\"")
        char '"'
        return $ String x
    where   escaped      = char '\\' >> choice (zipWith escapedChar codes replacements)
            escapedChar code replacement = char code >> return replacement
            codes        = ['b'  ,  'n'  ,  'f'  ,  'r'  ,  't'  ,  '\\' ,  '\"' ,  '/']
            replacements = ['\b' ,  '\n' ,  '\f' ,  '\r' ,  '\t' ,  '\\' ,  '\"' ,  '/']

parseChar :: Parser LispVal
parseChar = string "#\\" >> choice [space, newline, noneOf " ", empty] >>= return . Char
    where space   = string "space"   >> return ' '
          newline = string "newline" >> return '\n'
          empty   = return ' '

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest  <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
                     "#t" -> Bool True
                     "#f" -> Bool False
                     _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = try float <|> hex <|> oct <|> dec
    where hex = string "#x" >> many1 hexDigit >>= return . Number . liftNum . readHex
          oct = string "#o" >> many1 octDigit >>= return . Number . liftNum . readOct
          dec = many1 digit >>= return . Number . read
          float = do
              f <- many1 digit
              char '.'
              s <- many1 digit
              return $ Float $ liftNum $ readFloat (f ++ "." ++ s)
          liftNum [(val, rest)] = val 

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= return . List

parseDottedList :: Parser LispVal
parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail

parseVector :: Parser LispVal
parseVector = sepBy parseExpr spaces >>= return . Vector . V.fromList

parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseChar
        <|> parseBool
        <|> parseNumber
        <|> parseString
        <|> parseAtom
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
        <|> do char '['
               x <- try parseVector
               char ']'
               return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                            Left err  -> throwError $ Parser err
                            Right val -> return val

readExpr     = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

--  EVALUATE
instance Show LispVal where
        show = showVal

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (Float contents)       = show contents
showVal (Char contents)        = [contents]
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector contents)      = "[" ++ unwordsVector contents ++ "]"
showVal (Port _)               = "<IO port>"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (IOFunc _)             = "<IO primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing  -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unwordsVector :: V.Vector LispVal -> String
unwordsVector = unwords . V.toList . V.map showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)             = return val
eval env val@(Number _)             = return val
eval env val@(Bool _)               = return val
eval env val@(Atom id)              = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
        result <- eval env pred
        case result of
            Bool False -> eval env alt
            Bool True  -> eval env conseq
            otherwise  -> throwError $ TypeMismatch "If predicate must resolve to boolean " result
eval env (List [Atom "set!", Atom var, form])    =  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form])  =  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
        makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
        makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lamda" : List params : body)) =
        makeNormalFunc env params body
eval env (List (Atom "lamda" : DottedList params varargs : body)) =
        makeVarargs varargs env params body
eval env (List (Atom "lamda" : varargs @(Atom _) : body)) =
        makeVarargs varargs env [] body        
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
        func <- eval env function
        argVals <- mapM (eval env) args
        apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args        = func args
apply (Func params varargs body closure) args =
        if num params /= num args && varargs == Nothing
            then throwError $ NumArgs (num params) args
            else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env =
              case arg of
                  Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                  Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives
                                              ++ map (makeFunc IOFunc) ioPrimitives)
    where makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , ("<=", numBoolBinop (<=))
             , (">=", numBoolBinop (>=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)
             ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
--numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left  <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                        if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
                                     " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ 
                                          ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError

instance Show LispError where 
        show = showError

instance Error LispError where
        noMsg = Default "An error has occured"
        strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]              =  return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]          =  return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]          =  return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]              =  return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]  =  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]              =  return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                                 Left err         -> False
                                 Right (Bool val) -> val
eqv [_, _]                                  = return $ Bool False
eqv badArgList                              = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
        primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
        eqvEquals <- eqv [arg1, arg2]
        return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
--evalString env expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

util_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
util_ pred prompt action = do
        result <- prompt
        if pred result
            then return ()
            else action result >> util_ pred prompt action

--runOne :: String -> IO ()
--runOne expr = primitiveBindings >>= flip evalAndPrint expr
runOne :: [String] -> IO ()
runOne args = do
        env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
        (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
            >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= util_ (== "quit") (readPrompt "Lisp>> ") . evalAndPrint

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do alreadyDefined <- liftIO $ isBound envRef var
                                if alreadyDefined
                                    then setVar envRef var value >> return value
                                    else liftIO $ do valueRef <- newIORef value
                                                     env      <- readIORef envRef
                                                     writeIORef envRef ((var, valueRef) : env)
                                                     return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

--  IO Primitives

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
