module Lib
  ( getSum
  ) where

import Data.Char (ord)
import qualified Data.Map as DM
import qualified Data.Set as DS

getSum :: [String] -> Int
getSum [] = 0
getSum (rucksack:xs) = priority (common rucksack) + getSum xs

priority :: Char -> Int
priority c = memory DM.! c
  where
    memory =
      DM.fromList $
      concat [(zip ['a' .. 'z'] [1 .. 26]), (zip ['A' .. 'Z'] [27 .. 52])]

common :: String -> Char
common rucksack = (DS.toList $ DS.intersection fstHalf sndHalf) !! 0
  where
    halflen = length rucksack `div` 2
    fstHalf = DS.fromList $ take halflen rucksack
    sndHalf = DS.fromList $ drop halflen rucksack
