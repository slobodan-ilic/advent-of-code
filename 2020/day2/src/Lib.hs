module Lib
  ( nOkEntries
  ) where

import Data.Bits
import Data.List.Split

nOkEntries :: [String] -> Int
nOkEntries entries = sum [fromEnum $ isPassOK pass | pass <- entries]

isPassOK :: String -> Bool
isPassOK entry = (pass !! i1 == c) `xor` (pass !! i2 == c)
  where
    [indsToken, cToken, pass] = words entry
    (i1, i2) = getInds $ indsToken
    c = cToken !! 0

getInds :: String -> (Int, Int)
getInds token = (read strMin - 1, read strMax - 1)
  where
    [strMin, strMax] = splitOn "-" token
