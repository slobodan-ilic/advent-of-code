module Main
  ( main
  ) where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ groupPriorities $ lines contents
