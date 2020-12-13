module Lib
  ( readInput
  , runExample
  , findTime
  , findTime'
  , findNext
  , Bus
  ) where

import Data.List
import Data.List.Split

type BusId = Int

type Pos = Int

data Bus =
  Bus BusId Pos
  deriving (Show)

type Busses = [Bus]

type Time = Int

runExample :: String -> Time
runExample contents = findTime busses
  where
    busses = readInput contents

readInput :: String -> Busses
readInput contents =
  [Bus (read id :: Int) pos | (id, pos) <- zip strIds [0 ..], id /= "x"]
  where
    lns = lines contents
    strIds = splitOn "," $ lns !! 1

findTime :: Busses -> Time
findTime busses = findTime' 0 1 busses

findTime' :: Time -> Int -> Busses -> Time
findTime' time _ [] = time
findTime' time mult (bus@(Bus id pos):busses) = findTime' time' mult' busses
  where
    (time', mult') = findNext time mult bus

findNext :: Time -> Int -> Bus -> (Time, Int)
findNext time mult bus@(Bus id pos) =
  if timeRem == busRem
    then (time, mult * id)
    else findNext (time + mult) mult bus
  where
    busRem = (-pos) `mod` id
    timeRem = time `mod` id
