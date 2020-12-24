module Main where

import Lib

main :: IO ()
main = do
  cont <- readFile "input.txt"
  putStrLn $ show $ length $ play 100 $ process $ parse cont
  -- putStrLn $ show $ process $ parse cont
