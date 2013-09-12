module Main where

import System.Environment
import Data.List
import Data.Maybe

main :: IO ()
main = do
   n : _ <- getArgs
   putStrLn $ show $ difference $ read n

difference :: Int -> Int
difference n = (squareSum n) - (sumSquares n)

sumSquares :: Int -> Int
sumSquares n = sum $ map (^2) [1 .. n]

squareSum :: Int -> Int
squareSum n = (sum [1 .. n]) ^ 2


