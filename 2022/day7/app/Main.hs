module Main
  ( main
  ) where

import Data.Map as DM
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ part2 $ lines contents
  -- putStrLn $ show $ blockSize (blockMap "" $ lines contents) "/"
  -- putStrLn $ show $ blockSize (blockMap "" $ lines contents) "/a/e"
