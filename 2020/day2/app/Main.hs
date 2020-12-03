module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ nOkEntries (lines contents)
