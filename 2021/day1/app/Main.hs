module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show (nInc [read line | line <- lines contents])
