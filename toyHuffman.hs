import Data.List (insertBy, sortBy)
import Data.Char
import Data.Ord (comparing)
import qualified Data.Map as M
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)

data HTree a = Leaf a | Branch (HTree a) (HTree a)
                deriving Show

freq_list :: String -> [(Char, Int)]
freq_list = M.toList . M.fromListWith (+) . map (flip (,) 1)

-- cws = (chars,weights)
pretrees :: (Ord a1, Num a1) => [(a2, a1)] -> HTree a2
pretrees cws = htree $ sortBy (comparing fst) $ [(w, Leaf c) | (c,w) <- cws]       

make_htree :: String -> HTree Char
make_htree = pretrees . freq_list

make_codebank :: String -> [(Char, [Char])]
make_codebank = sortBy (comparing fst) . serialize . make_htree

htree :: (Ord a1, Num a1) => [(a1, HTree a2)] -> HTree a2
htree [(_, t)]              = t
htree ((w1,t1):(w2,t2):wts) = htree $ insertBy (comparing fst) 
                              (w1 + w2, Branch t1 t2) wts

serialize :: HTree a -> [(a, [Char])]
serialize (Branch l r) =
  [(x, '0':o)  | (x, o) <- serialize l] ++
  [(x, '1':i)  | (x, i) <- serialize r]
serialize (Leaf x) = [(x, "")]

compress :: String -> String
compress string = concat $ map (code M.!) string
  where code = M.fromList $ make_codebank string

decodeString :: HTree a -> String -> [a]
decodeString root = go root
 where go (Branch l r) (x:xs) = case x of
          '0' -> go l xs
          '1' -> go r xs
       go (Leaf c) xs = c : go root xs
       go _ []        = []

encodeLine :: IO ()
encodeLine = do 
  putStrLn "Enter string:"
  str <- getLine
  let compressed = compress str
  putStr "Huffman coding: "
  print compressed

encodeOnlineText = do
  putStr "Enter URL: "
  src <- getLine
  rsp <- simpleHTTP (getRequest src)
  html <- fmap (takeWhile isAscii) (getResponseBody rsp)
  print $ make_codebank html

-- try "http://norvig.com/big.txt"

encodeTextFile = do 
  txt <- readFile "text.txt"
  print $ make_codebank txt

