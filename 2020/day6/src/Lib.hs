module Lib
  ( nSameAnswers
  , nAnswered
  , unique
  , unique'
  , getGroups
  ) where

import Data.List.Split

nSameAnswers :: [String] -> Int
nSameAnswers lines =
  sum
    [ length (filter (== groupSize) tAnsLine)
    | (tAnsLine, groupSize) <- zip timesAnweredLines groupSizes
    ]
  where
    groups = getGroups lines
    groupSizes = map length groups
    tokens = map concat groups
    uniques = map unique tokens
    timesAnweredLines = map (uncurry nAnswered) (zip uniques tokens)

getGroups :: [String] -> [[String]]
getGroups lines = splitOn [""] lines

unique :: String -> String
unique x = unique' [] x

unique' :: String -> String -> String
unique' acc [] = acc
unique' acc (x:xs) =
  case elem x acc of
    True -> unique' acc xs
    _ -> unique' (x : acc) xs

nAnswered :: String -> String -> [Int]
nAnswered [] _ = []
nAnswered (x:xs) token = length (filter (== x) token) : nAnswered xs token
