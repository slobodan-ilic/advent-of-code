module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ getWeakCode (numbers $ lines contents) 675280050
-- 675280050
