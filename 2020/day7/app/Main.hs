module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ getTotalBags (map processRule (lines contents)) "shinygold"
