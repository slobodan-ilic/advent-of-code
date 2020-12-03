module Lib
  ( treesProd
  ) where

treesProd :: [String] -> [(Int, Int)] -> Int
treesProd lines slopes = product $ map (\slope -> nTrees lines slope) slopes

nTrees :: [String] -> (Int, Int) -> Int
nTrees lines slope = count 0 lines slope slope

count :: Int -> [String] -> (Int, Int) -> (Int, Int) -> Int
count acc lines (x, y) (xd, yd)
  | x >= length lines = acc
  | otherwise = count (acc + inc) lines newPos (xd, yd)
  where
    inc = fromEnum $ hit lines (x, y)
    newPos = (x + xd, y + yd)

hit :: [String] -> (Int, Int) -> Bool
hit lines (x, y) = (lines !! i) !! j == '#'
  where
    i = x `mod` (length lines)
    j = y `mod` (length $ lines !! 0)
