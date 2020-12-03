module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ treesProd (lines contents) slopes

slopes :: [(Int, Int)]
slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
