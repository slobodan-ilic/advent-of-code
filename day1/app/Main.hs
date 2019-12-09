module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  displayResult $ calculateFuelFrom contents
