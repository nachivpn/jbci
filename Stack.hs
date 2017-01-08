module Stack where

data Stack a = Stack' [a] deriving Show

emptyStack :: Stack a
emptyStack = Stack' [] 

push :: a -> Stack a -> Stack a
push x (Stack' xs) = Stack' (x:xs)
 
pop :: Stack a -> (a,Stack a)
pop (Stack' (x:xs)) = (x,Stack' xs)
