module Main where

import System.Environment
import Data.List
import Control.Applicative
import Data.Maybe

main :: IO ()
main = do
   n : _ <- getArgs
   putStrLn $ show $ smallestMultiple (read n)

smallestMultiple :: Int -> Int
smallestMultiple n = foldl (*) 1 (map (minExponent n) (primes n))

-- find the biggest k so that p^k <= n
-- and return p^k
minExponent :: Int -> Int -> Int
minExponent n p = p ^ (floor ((log (fromIntegral n) / (log (fromIntegral p)))))

primes :: Int -> [Int]
primes n = 2 : turner [3, 5 .. n] where
  turner []       = []
  turner (p : xs) = p : turner [x | x <- xs, rem x p /= 0]



