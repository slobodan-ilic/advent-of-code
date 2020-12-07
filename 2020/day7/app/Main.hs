module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $
    show $ length $ getPaths (map processRule (lines contents)) ["shinygold"]
