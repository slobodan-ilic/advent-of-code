module Lib
  ( part1
  , part2
  , seesLeft
  , digits
  ) where

import Data.Char
import qualified Data.List as DL
import qualified Data.List.Split as SP

seesLeft :: Int -> [Int] -> [(Int, Int)]
seesLeft add [] = []
seesLeft add row = DL.intercalate sees (map (seesLeft 1) parts)
  where
    m = maximum row
    parts = SP.splitOn [m] row
    lens = map length parts
    lens' =
      (add + head lens) : (map (+ 1) (init (tail lens))) ++ [(add + last lens)]
    sees = zip (init lens') (tail lens')

part1 :: [String] -> Int
part1 lns =
  sum
    [ if v
      then 1
      else 0
    | v <- visibles
    ]
  where
    mapL2R = mapVis lns
    mapR2L = map reverse $ mapVis $ map reverse lns
    mapU2D = DL.transpose $ mapVis $ DL.transpose lns
    mapD2U =
      DL.transpose $ map reverse $ mapVis $ map reverse $ DL.transpose lns
    mapFin =
      DL.zip4 (concat mapL2R) (concat mapR2L) (concat mapU2D) (concat mapD2U)
    visibles = map (\(a, b, c, d) -> a || b || c || d) mapFin

-- part2 :: [String] -> [[(Bool, Bool, Bool, Bool)]]
part2 lns = maximum finScores
  where
    mtx = digits lns
    scores =
      [ (i, j, visL mtx i j, visR mtx i j, visU mtx i j, visD mtx i j)
      | i <- [0 .. (length mtx - 1)]
      , j <- [0 .. (length (mtx !! 0) - 1)]
      ]
    finScores = map (\(i, j, a, b, c, d) -> a * b * c * d) scores

mapVis :: [String] -> [[Bool]]
mapVis [] = []
mapVis (x:xs) = lineVis x : mapVis xs

lineVis :: String -> [Bool]
lineVis line = vis
  where
    digits = [digitToInt c | c <- line]
    vis = zipWith (>) digits $ scanl max (-1) digits

digits :: [String] -> [[Int]]
digits [] = []
digits (x:xs) = map digitToInt x : digits xs

visL :: [[Int]] -> Int -> Int -> Int
visL _ _ 0 = 0
visL mtx i j = score
  where
    row = mtx !! i
    el = row !! j
    leftPart = take j row
    score = min j $ 1 + length (takeWhile (< el) $ reverse leftPart)

visR :: [[Int]] -> Int -> Int -> Int
visR mtx i j = visL [reverse row | row <- mtx] i j'
  where
    j' = length (mtx !! 0) - j - 1

visU mtx i j = visL (DL.transpose mtx) j i

visD mtx i j = visR (DL.transpose mtx) j i
-- visD mtx i j = visL [reverse row | row <- DL.transpose mtx] i j
