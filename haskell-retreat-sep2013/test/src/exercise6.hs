module Main where

import System.Environment
import Data.List
import Data.Maybe
import Data.Char
import Data.List

main :: IO ()
main = do
   n : _ <- getArgs
   putStrLn $ show $ firstTriangleNumberWithMoreDivisorsThan (read n)

firstTriangleNumberWithMoreDivisorsThan :: Int -> Int
firstTriangleNumberWithMoreDivisorsThan n = fromMaybe 0 (fmap fst (find ((> n) . snd) divisorsNbOfTriangleNumbers))

divisorsNbOfTriangleNumbers :: [(Int, Int)]
divisorsNbOfTriangleNumbers = map divisorsNb triangleNumbers

triangleNumbers :: [Int]
triangleNumbers = map triangle [1 ..]

triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2

divisorsNb :: Int -> (Int, Int)
divisorsNb n = (n, (length . divisors) n)

divisors :: Int -> [Int]
divisors n = filter (\x -> (rem n x) == 0) [1 .. n]