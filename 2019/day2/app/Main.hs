module Main where

import Control.Monad.State
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ head $ evalState (intcode 0) (getOps contents)
