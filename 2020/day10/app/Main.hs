module Main where

import Data.List
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $
    show $
    combinations $
    (0 : (sort $ map (\line -> read line :: Int) (lines contents)))
