module Main where

import System.Environment
import Data.List
import Data.Maybe

main :: IO ()
main = do
   s : _ <- getArgs
   putStrLn $ encode s

encode :: String -> String
encode string =  string >>= encodeChar

encodeChar :: Char -> String
encodeChar c = fromMaybe [c] (findCorresponding c)

findCorresponding :: Char -> Maybe String
findCorresponding c = lookup c [('+', "%2B"), (' ', "%20"), ('#', "%23"), ('%', "%25")]



