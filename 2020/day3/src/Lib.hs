module Lib
  ( nTrees
  , hit
  ) where

nTrees :: [String] -> Int
nTrees lines = count 0 lines (1, 3)

count :: Int -> [String] -> (Int, Int) -> Int
count acc lines (x, y)
  | x >= length lines = acc
  | otherwise = count (acc + inc) lines newPos
  where
    inc = fromEnum $ hit lines (x, y)
    newPos = (x + 1, y + 3)

hit :: [String] -> (Int, Int) -> Bool
hit lines (x, y) = (lines !! i) !! j == '#'
  where
    i = x `mod` (length lines)
    j = y `mod` (length $ lines !! 0)
