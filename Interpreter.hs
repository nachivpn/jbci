import Stack
import Storage
import Parsing
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

-- State contains the program counter, variable storage and the stack
type State = (Int, Storage, Stack Integer)

data Instruction = BIPUSH Integer | IADD | IMUL | ILOAD Int
                 | ISTORE Int | GOTO String | IFEQ String 
                 | LABEL String
                 deriving (Show, Eq, Ord)

-- eval evaluates an instruction given the state of the machine
-- and a label environment containing label and corresponding locations in the program
-- the results of the evaluation are reflected in the resulting state
eval :: Instruction -> State -> Map.Map Instruction Int -> State
eval (BIPUSH i) (p,v,s) le = (p + 1, v, push i s)
eval (IADD) (p,v,s) le = let (y,s') = pop s
                             (x,s'') = pop s'
                          in (p + 1, v, push (x+y) s'')
eval (IMUL) (p,v,s) le = let (y,s') = pop s
                             (x,s'') = pop s'
                          in (p + 1, v, push (x*y) s'')
eval (ILOAD i) (p,v,s) le = (p + 1, v, push (read $ get v i) s)
eval (ISTORE i) (p,v,s) le = let (x,s') = pop s
                              in (p+1, store v i 1 (show x), s')
eval (GOTO l) (p,v,s) le = let p' = lookupLabelLoc l le
                            in (p', v, s) 
eval (LABEL l) (p,v,s) le = (p + 1, v, s)
eval (IFEQ l) (p,v,s) le = let (x,s') = pop s
                               p' = lookupLabelLoc l le
                            in if x==0 
                                  then (p', v, s')
                                  else (p+1,v,s')

-- Parses the program, runs the interpreter
-- prints the value remaining on top of the stack
main :: IO()
main = do
    code <- readFile "code.j"
    let maybei = parse' code
    case maybei of
      (Nothing) -> putStr "Syntax error, parsing failed"
      (Just instructions) -> print $ interpret instructions

interpret :: [Instruction] -> Integer
interpret is = fst $ pop s
    where (p,v,s) = interpret' is (0, emptyStorage, emptyStack) (labelEnv is)

interpret' :: [Instruction] -> State -> Map.Map Instruction Int -> State
interpret' is state@(pc,vars,stack) le 
  | pc < 0 = state
  | pc >= length is = state
  | otherwise = interpret' is (eval (is!!pc) state le) le

-----------------------------------------------------------
-- PARSER --
-----------------------------------------------------------
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

posint :: Parser Integer
posint = do
    s <- oneOrMore digit
    return (read s)

negint :: Parser Integer
negint = do
    neg <- char '-' 
    s <- oneOrMore digit
    return $ (read s) * (-1)

int :: Parser Integer
int = posint <|> negint

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
    int

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

------------------------------------------------------------
-- LABEL environment functions --
------------------------------------------------------------
isLabel :: Instruction -> Bool
isLabel (LABEL a) = True
isLabel _ = False

getLabelIndices = findIndices isLabel

-- index labels in the given instruction list
labelEnv :: [Instruction]-> Map.Map Instruction Int
labelEnv is = Map.fromList [(is!!index, index) | index<-labelIndices]
    where labelIndices = getLabelIndices is

-- lookup the position of a label in the program using it's name
lookupLabelLoc :: String -> Map.Map Instruction Int ->  Int
lookupLabelLoc l m = lookupLabelLoc' $ Map.lookup (LABEL l) m
    where 
        lookupLabelLoc' Nothing = error "no such label" 
        lookupLabelLoc' (Just x) = x
