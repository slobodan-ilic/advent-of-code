module Lib
  ( parseInput
  , generateList
  ) where

import qualified Data.IntMap.Strict as Map
import Data.List
import Data.List.Split

type Memory = Map.IntMap Int

parseInput :: String -> [Int]
parseInput str = map (\el -> read el :: Int) nums
  where
    nums = splitOn "," str

generateList :: Int -> [Int] -> Int
generateList nMax starting =
  generateList' initMap initCounter nMax (last starting)
  where
    initMap =
      Map.fromList $ init [(key, val) | (key, val) <- zip starting [0 ..]]
    initCounter = length starting

generateList' :: Memory -> Int -> Int -> Int -> Int
generateList' memory counter nMax current
  | counter >= nMax = current
  | otherwise =
    generateList' (Map.insert current def memory) (counter + 1) nMax next
  where
    def = counter - 1
    next = def - (Map.findWithDefault def current memory)
