module Main
  ( main
  ) where

import qualified Data.List as DL
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let mvs = moves $ lines contents
      start = take 10 $ repeat (0, 0)
      positions = navStates [start] mvs
      tails = map last positions
      unique = DL.nub tails
   in putStrLn $ show $ length unique
   -- in mapM_ (putStrLn . show) $ reprState $ positions !! (5 + 8 + 8)
