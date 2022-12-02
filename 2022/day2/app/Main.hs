module Main
  ( main
  ) where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ process $ lines contents
  -- putStrLn $ show $ lines contents
