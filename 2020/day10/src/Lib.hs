module Lib
  ( combinations
  ) where

import Data.List.Split

type Adapter = Int

type Adapters = [Adapter]

type Difference = Int

type Segment = [Difference]

combinations :: Adapters -> Int
combinations adapters = product $ map nPaths segments
  where
    diffs = diffSegment adapters
    segments = filter (/= []) $ splitOn [3] diffs

nPaths :: Segment -> Int
nPaths (d1:[]) = fromEnum $ d1 <= 3
nPaths (d1:d2:xs)
  | d1 == 3 = nRest
  | d1 < 3 = nSkipNext + nRest
  | otherwise = 0
  where
    nRest = nPaths (d2 : xs)
    nSkipNext = nPaths ((d1 + d2) : xs)

diffSegment :: Adapters -> Segment
diffSegment (a:[]) = []
diffSegment (a:b:as) = (b - a) : (diffSegment (b : as))
