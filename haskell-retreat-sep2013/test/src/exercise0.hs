module Main where

import System.Environment
import Data.List

main = do
   name : _ <- getArgs
   putStrLn ("Hello " ++ name)
