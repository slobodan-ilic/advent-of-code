module Lib where

import Data.List

type Cups = [Int]

present :: Cups -> String
present cups = show $ second ++ first
  where
    i = destInd [] cups 1
    first = take i cups
    second = drop (i + 1) cups

play :: Int -> Cups -> Cups
play n cups =
  if n > 0
    then play (n - 1) $ move cups
    else cups

move :: Cups -> Cups
move cups = cups'
  where
    c = head cups
    pick = take 3 $ drop 1 cups
    rest = drop 4 cups -- check if there are bugs
    i = destInd (c : pick) rest c
    cups' = take (i + 1) rest ++ pick ++ drop (i + 1) rest ++ [c]

destInd :: Cups -> Cups -> Int -> Int
destInd picked cups t =
  case ind of
    (Just i) -> i
    Nothing ->
      if t' `elem` (cups ++ picked)
        then destInd picked cups t'
        else (t' - 1)
  where
    ind = elemIndex t cups
    t' =
      if t < minimum cups
        then maximum cups
        else t - 1
