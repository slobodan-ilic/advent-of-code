module Main where

import Data.List
import qualified Data.Map as M
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let pd0 = initState $ concat $ parse $ lines contents
      pd1 = executeCycle pd0
      pd2 = executeCycle pd1
      pd3 = executeCycle pd2
      pd4 = executeCycle pd3
      pd5 = executeCycle pd4
      pd6 = executeCycle pd5
      n =
        M.foldr (\(PocketCube _ s) acc -> acc + (fromEnum $ s == Active)) 0 pd6
      -- s' = changeCubeState pd0 (PocketCube (1, 0, 0, 0) Inactive)
  putStrLn $ show $ n
