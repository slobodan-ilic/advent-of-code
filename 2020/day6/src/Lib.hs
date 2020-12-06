module Lib
  ( nAnswers
  , unique
  , unique'
  , tokens
  ) where

import Data.List.Split

nAnswers :: [String] -> Int
nAnswers lines = sum $ map (length . unique) $ tokens lines

tokens :: [String] -> [String]
tokens lines = map concat $ splitOn [""] lines

unique :: String -> String
unique x = unique' [] x

unique' :: String -> String -> String
unique' acc [] = acc
unique' acc (x:xs) =
  case elem x acc of
    True -> unique' acc xs
    _ -> unique' (x : acc) xs
