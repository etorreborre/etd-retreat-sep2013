module Main where

import System.Environment
import Data.List
import Data.Maybe
import Data.Char
import Data.List

main :: IO ()
main = do
   digits : _ <- getArgs
   putStrLn $ show $ greatestProduct (map digitToInt digits)

greatestProduct :: [Int] -> Int
greatestProduct list = maximum (map listProduct (slide list 5))

listProduct :: [Int] -> Int
listProduct = foldl (*) 1

slide :: [a] -> Int -> [[a]]
slide list n = map (take n) (duplicate list)

duplicate :: [a] -> [[a]]
duplicate list = init $ tails list
