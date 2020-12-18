module Lib where

import Data.List
import Data.List.Split

plus :: Int -> Int -> Int
plus a b = a + b

mul :: Int -> Int -> Int
mul a b = a * b

transformExpr :: String -> String
transformExpr contents = intercalate "`plus`" $ splitOn "+" line
  where
    line = "(" ++ (intercalate ") + (" (lines contents)) ++ ")"
