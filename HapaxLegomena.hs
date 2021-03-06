-- Takes a text file and uses set membership relations to make a list of the hapax legomena, the words that
-- only occur once. 

import Data.Char
import Data.List
import System.Environment
import System.Directory
import System.IO
import qualified Data.Set as Set


main = do
  putStrLn "Here are the hapax legomena"
  contents <- readFile "yourfilegoeshere.txt"
  let wordlist  = map toLower contents
      wordlist1 = map noPunc $ words wordlist
      wordlist2 = sort $ unique wordlist1
  print wordlist2

-- convert contents into a list of words. get relative frequency list and and zip them to make tuples

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

frequency :: [String] -> [Float]
frequency words' = [percent (occs word) totalwords | word <- words']
        where occs word = length (filter (==word) words')
              totalwords = length words'

-- count number of times p occurs in [a]. (recursion + pattern matching with boolean guards)
count :: Eq a => a -> [a] -> Int
count p []                 = 0
count p (x:xs) | p == x    = 1 + count p xs
               | otherwise = count p xs

-- 
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | x `notElem` xs     = x : unique xs
  | otherwise          = unique xs

noPunc xs = [ x | x <- xs, x `elem` "abcdefghijklmnopqrstuvwxyz" ]
