module Main
  ( main
  ) where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ show $ part2 $ lines contents

-- demo = seesLeft [2, 1, 1, 2, 1, 2, 2]
demo = seesLeft 0 [1, 1]
