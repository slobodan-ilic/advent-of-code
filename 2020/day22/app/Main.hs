module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let game = parse contents
   in putStrLn $ show $ (game, calc $ play game)
