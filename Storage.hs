module Storage(Storage, emptyStorage, get, store) where

type Range = (Int, Int)
type Block = (Range,String)

data Storage = Storage' [Block] deriving Show

emptyStorage :: Storage
emptyStorage = Storage' []

store :: Storage -> Int -> Int -> String -> Storage
store s i l v = if(isFree s i l)
                   then addBlock s i (i+l-1) v 
                   else error "Segmentation fault: not free"

get :: Storage -> Int -> String
get (Storage' []) _ = error "Segmentation fault: Invalid access, not allocated"
get (Storage' (b:bs)) i 
  | startBlock b == i = getBlockValue b
  | otherwise = get (Storage' bs) i

--TODO
isFree :: Storage -> Int -> Int -> Bool
isFree s i l = True

startBlock :: Block -> Int
startBlock = fst.fst

endBlock :: Block -> Int
endBlock = snd.fst

getBlockValue :: Block -> String
getBlockValue = snd

addBlock :: Storage -> Int -> Int -> String -> Storage
addBlock (Storage' bs) x y v = Storage' $ ((x,y),v):bs
