module Main where

import Data.List
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ getMinDistance contents
