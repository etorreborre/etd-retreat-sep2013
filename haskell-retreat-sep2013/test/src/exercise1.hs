module Main where

import System.Environment
import Data.List
import Control.Applicative
import Data.Maybe

main :: IO ()
main = do
   n : m : _ <- getArgs
   putStrLn $ show $ largestPalindrome (read n) (read m)

largestPalindrome :: Int -> Int -> (Int, Int, Int)
largestPalindrome n m = fromMaybe (0, 0, 0) $ find (isPalindrome . fst3) (products n m)

fst3 :: (a, a, a) -> a
fst3 (a, _, _) = a

isPalindrome :: Int -> Bool
isPalindrome n = reverse (show n) == (show n)

products :: Int -> Int -> [(Int, Int, Int)]
products n m = multiplyLists (reverse [1 .. n]) (reverse [1 .. m])

multiplyLists :: [Int] -> [Int] -> [(Int, Int, Int)]
multiplyLists l1 l2 = multiply <$> l1 <*> l2

multiply :: Int -> Int -> (Int, Int, Int)
multiply n m = (n * m, n, m)


