module Lib
  ( countPaths
  ) where

import Data.List.Split

type Adapter = Int

type Adapters = [Adapter]

type Difference = Int

type Segment = [Difference]

countPaths :: Adapters -> Int
countPaths adapters = product $ map nPaths segments
  where
    diffs = diffSegment adapters
    segments = splitOn [3] diffs

nPaths :: Segment -> Int
nPaths segment =
  case length segment of
    4 -> 7
    3 -> 4
    2 -> 2
    _ -> 1

diffSegment :: Adapters -> Segment
diffSegment (a:[]) = []
diffSegment (a:b:as) = (b - a) : (diffSegment (b : as))
