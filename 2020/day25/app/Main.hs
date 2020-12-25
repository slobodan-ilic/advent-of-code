module Main where

import Lib

main :: IO ()
main = do
  putStrLn $ show $ transformSubjectNumber 9 7
