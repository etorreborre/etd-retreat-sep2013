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
   n : _ <- getArgs
   putStrLn $ show $ sum $ palindromesIn2Bases (read n)

palindromesIn2Bases :: Int -> [Int]
palindromesIn2Bases n = filter ((&&) <$> isPalidromeInBase2 <*> isPalidromeInBase10) [1 .. n]

isPalidromeInBase2 :: Int -> Bool
isPalidromeInBase2 = isPalindrome . toBase2

isPalidromeInBase10 :: Int -> Bool
isPalidromeInBase10 = isPalindrome

toBase2 :: Int -> Int
toBase2 n = read $ showIntAtBase 2 intToDigit n ""

isPalindrome :: Int -> Bool
isPalindrome = (==) <$> reverse . show <*> show
