module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show (getProduct3 $ [read line | line <- lines contents])
