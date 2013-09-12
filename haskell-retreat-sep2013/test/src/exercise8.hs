module Main where

import System.Environment
import Data.List
import Data.Maybe
import Data.Char
import Data.List
import Control.Applicative
import Numeric

main :: IO ()
main = do
   maxWeight : items <- getArgs
   putStrLn $ show $ knapsack (read maxWeight) (pairs (map read items)) 0

-- main knapsack function
knapsack ::
  Int ->      -- maximum weight
  [Item] ->   -- current list of items
  Int ->      -- current best
  Int         -- final result
knapsack _ [] _ = 0
knapsack maxWeight items best =
  let values = map (\c -> knapsack (maxWeight - weight c) (items `minus` c) (best + (value c))) (candidates maxWeight items) in
    if (null values) then best
    else maximum values

-- find the possible candidates which are at least less than the maximum weight
candidates :: Int -> [Item] -> [Item]
candidates maxWeight = filter ((<= maxWeight) . weight)

-- return pairs from elements in a list, grouped 2 by 2
pairs :: [a] -> [(a, a)]
pairs []           = []
pairs (_ : [])     = []
pairs (x : y : xs) = (x, y) : (pairs xs)

-- remove an element from a list
minus :: Eq a => [a] -> a -> [a]
minus xs a = filter (/= a) xs

-- quick modelling for an Item with a value and a weight
type Item = (Int, Int)

weight :: Item -> Int
weight = snd

value :: Item -> Int
value = fst