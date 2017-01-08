import Stack
import Storage
import Parsing
import Data.List
import Data.Char
import Data.Maybe

type State = (Int, Storage, Stack Integer)

data Instruction = BIPUSH Integer | IADD | IMUL | ILOAD Int
                 | ISTORE Int | GOTO String | IFEQ String 
                 | LABEL String
                 deriving (Show, Eq)

eval :: Instruction -> State -> State
eval (BIPUSH i) (p,v,s) = (p + 1, v, push i s)
eval (IADD) (p,v,s) = let (y,s') = pop s
                          (x,s'') = pop s'
                       in (p + 1, v, push (x+y) s'')
eval (IMUL) (p,v,s) = let (y,s') = pop s
                          (x,s'') = pop s'
                       in (p + 1, v, push (x*y) s'')
eval (ILOAD i) (p,v,s) = (p + 1, v, push (read $ get v i) s)
eval (ISTORE i) (p,v,s) = let (x,s') = pop s
                           in (p+1, store v i 1 (show x), s')

main :: IO()
main = do
    code <- readFile "code.j"
    let maybei = parse' code
    case maybei of
      (Nothing) -> putStr "Syntax error, parsing failed"
      (Just instructions) -> print $ interpret instructions

interpret :: [Instruction] -> Integer
interpret is = fst $ pop s
    where (p,v,s) = interpret' is (0, emptyStorage, emptyStack)  

interpret' :: [Instruction] -> State -> State
interpret' is state@(pc,vars,stack) 
  | pc < 0 = state
  | pc >= length is = state
  | otherwise = interpret' is (eval (is!!pc) state)

parse' :: String -> Maybe [Instruction]
parse' code = maybeFst [ parse instruction i | i<-(preparse code)]

preparse :: String -> [String]
preparse = (filter (not.null)).(map trim).lines
    where trim = f . f
          f = reverse . dropWhile isSpace

maybeFst :: (Eq a, Eq b) => [Maybe (a,b)] -> Maybe [a]
maybeFst xs 
  | Nothing `elem` xs = Nothing
  | otherwise = Just (map (fst.fromJust) xs)

int :: Parser Integer
int = do
    s <- oneOrMore digit
    return (read s)

double :: Parser Double
double = do
    a <- oneOrMore digit
    d <- char '.'
    b <- oneOrMore digit
    return (read $ a ++ [d] ++ b)

command :: Parser String
command = oneOrMore (sat isAlpha)

iarg :: Parser Integer
iarg = do
    _ <- oneOrMore (sat isSpace)
    read `fmap` oneOrMore digit

larg :: Parser String
larg = do
    _ <- oneOrMore (sat isSpace)
    oneOrMore (sat (\x -> isAlpha x || isNumber x))

instruction :: Parser Instruction
instruction = do
    _ <- zeroOrMore (sat isSpace)
    c <- command
    case c of 
        "bipush" -> do
            a <- iarg
            return (BIPUSH a)
        "iadd" -> return (IADD)
        "imul" -> return (IMUL)
        "istore" -> do
            a <- iarg
            return (ISTORE (fromIntegral a))
        "iload" -> do
            a <- iarg
            return (ILOAD (fromIntegral a))
        "goto" -> do
            a <- larg
            return (GOTO a)
        "ifeq" -> do
            a <- larg
            return (IFEQ a)
        _ -> do 
            char ':'
            return (LABEL c)
