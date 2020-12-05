module Main where

import Data.List
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ findMySeat $ sort $ map getSeatId (lines contents)
