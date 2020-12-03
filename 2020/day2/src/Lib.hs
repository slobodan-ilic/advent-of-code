module Lib
  ( nOkEntries
  ) where

import Data.List.Split

nOkEntries :: [String] -> Int
nOkEntries entries = sum [fromEnum $ isPassOK pass | pass <- entries]

isPassOK :: String -> Bool
isPassOK entry = n >= min && n <= max
  where
    tokens = words entry
    (min, max) = getLimits $ tokens !! 0
    char = (tokens !! 1) !! 0
    n = foldr (\el acc -> acc + fromEnum (el == char)) 0 (tokens !! 2)

getLimits :: String -> (Int, Int)
getLimits token = (read strMin, read strMax)
  where
    [strMin, strMax] = splitOn "-" token
