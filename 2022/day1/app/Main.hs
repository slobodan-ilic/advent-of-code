module Main
  ( main
  ) where

import Data.List.Split
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lsts = splitOn [""] $ lines contents
   in putStrLn $ show $ getMax lsts
