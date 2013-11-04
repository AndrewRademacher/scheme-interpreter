
{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Vector                                    as V
import           Text.ParserCombinators.Parsec hiding (spaces)
import           System.Environment
import           Numeric
import           Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (V.Vector LispVal)
             | Number Integer
             | Float Double
             | String String
             | Char Char
             | Bool Bool

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
--main = getArgs >>= print . eval . readExpr . head
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                     Left err  -> throwError $ Parser err
                     Right val -> return val

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unwordsVector :: V.Vector LispVal -> String
unwordsVector = unwords . V.toList . V.map showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
        result <- eval pred
        case result of
            Bool False -> eval alt
            Bool True  -> eval conseq
            otherwise  -> throwError $ TypeMismatch "If predicate must resolve to boolean " result
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args " func) 
                        ($ args) 
                        (lookup func primitives)

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
