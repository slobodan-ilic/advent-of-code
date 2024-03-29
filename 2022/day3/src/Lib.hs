module Lib
  ( getSum
  , groupPriorities
  ) where

import Data.Char (ord)
import qualified Data.Map as DM
import qualified Data.Set as DS

getSum :: [String] -> Int
getSum [] = 0
getSum (rucksack:xs) = priority (common rucksack) + getSum xs

groupPriorities :: [String] -> Int
groupPriorities [] = 0
groupPriorities (a:b:c:xs) =
  priority (groupCommon [a, b, c]) + groupPriorities xs

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

groupCommon :: [String] -> Char
groupCommon (a:b:c:[]) = (DS.toList commons) !! 0
  where
    commons1 = DS.intersection (DS.fromList a) (DS.fromList b)
    commons = DS.intersection (DS.fromList c) commons1
