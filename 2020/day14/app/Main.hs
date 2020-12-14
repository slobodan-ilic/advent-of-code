module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ calculate $ run $ parseInput contents
  -- putStrLn $ show $ parseInput contents
