module Main where

import System.Environment
import Data.List
import Data.Maybe
import Data.Char
import Data.List
import Data.Tuple
import Control.Applicative
import Numeric

main :: IO ()
main = do
   input : _ <- getArgs
   putStrLn $ literally input

literally :: String -> String
literally input = let (m, d) = (verbatimDollars (mainPart input), verbatimCents (decimalPart input)) in
  m++" dollars"++(cents d)

cents :: String -> String
cents d = if (null d) then "" else " and "++d++" cents"

verbatimCents :: Int -> String
verbatimCents i = if (i <= 20) then fromMaybe "" (lookup i numbers)
                  else if ((rem i 10) == 0) then fromMaybe "" (lookup i numbers)
                  else let r = rem i 10 in
                         (fromMaybe "" (lookup (i - r) numbers))++"-"++
                         (fromMaybe "" (lookup r numbers ))

verbatimDollars :: Int -> String
verbatimDollars i = let (l, s) = (verbatimLarge i, verbatimSmall i) in
  (if (null s) then l else l++" and "++s)++" dollars"

verbatimLarge :: Int -> String
verbatimLarge i = intercalate " and " (map makeLarge (exponents i))

exponents :: Int -> [(Int, Int)]
exponents n = reverse $ dropWhile ((== 0) . fst) (zipWithIndex (map (\c -> read [c]) (show n)))

makeLarge :: (Int, Int) -> String
makeLarge (qty, ex) = (number  qty)++" "++(largeNumber ex)

number :: Int -> String
number n = fromMaybe "" (lookup n numbers)

largeNumber :: Int -> String
largeNumber n = fromMaybe "" (lookup n (map swap (zipWithIndex largeNumbers)))

large :: Int -> String
large i = reverse (drop 2 (reverse (show i)))

verbatimSmall :: Int -> String
verbatimSmall = number

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex xs = zip xs [0 ..]

mainPart :: String -> Int
mainPart s = read $ takeWhile (/= '.') s

decimalPart :: String -> Int
decimalPart s = read $ drop 1 (dropWhile (/= '.') s)

numbers :: [(Int, String)]
numbers = [ (0, "zero")
           ,(1, "one")
           ,(2, "two")
           ,(3, "three")
           ,(4, "four")
           ,(5, "five")
           ,(6, "six")
           ,(7, "seven")
           ,(8, "eight")
           ,(9, "nine")
           ,(10, "ten")
           ,(11, "eleven")
           ,(12, "twelve")
           ,(13, "thirteen")
           ,(14, "fourteen")
           ,(15, "fifteen")
           ,(16, "sixteen")
           ,(17, "seventeen")
           ,(18, "eighteen")
           ,(19, "nineteen")
           ,(20, "twenty")
           ,(30, "thirty")
           ,(40, "forty")
           ,(50, "fifty")
           ,(60, "sixty")
           ,(70, "seventy")
           ,(80, "eighty")
           ,(90, "ninety")
          ]

largeNumbers :: [String]
largeNumbers = [   "hundred"
                 , "thousand"
                 , "million"
                 , "billion"
                 , "trillion"
                 , "quadrillion"
                 , "quintillion"
                 , "sextillion"
                 , "septillion"
                 , "octillion"
                 , "nonillion"
                 , "decillion"
                 , "undecillion"
                 , "duodecillion"
                 , "tredecillion"
                 , "quattuordecillion"
                 , "quindecillion"
                 , "sexdecillion"
                 , "septendecillion"
                 , "octodecillion"
                 , "novemdecillion"
                 , "vigintillion"
                 ]