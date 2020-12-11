module Lib
  ( readMap
  , atPos
  , getSurrounding
  , updateAtPos
  , updateState
  , countOccupied
  , process
  , getSize
  , getFromDirection
  , getVisible
  ) where

import Data.List.Split

data Seat
  = Floor
  | Empty
  | Occupied
  deriving (Show, Eq)

type SittingArea = [[Seat]]

type Pos = Int

type Coord = (Int, Int)

type Direction = (Int, Int)

process :: SittingArea -> Int
process seats =
  if seats == updatedSeats
    then countOccupied seats
    else process updatedSeats
  where
    updatedSeats = updateState size seats
    size = getSize seats

countOccupied :: SittingArea -> Int
countOccupied seats = length $ filter (== Occupied) $ concat seats

updateState :: (Int, Int) -> SittingArea -> SittingArea
updateState (m, n) seats = chunksOf n new
  where
    new = map (\curr -> updateAtPos (m, n) seats curr) [0 .. (m * n) - 1]

updateAtPos :: (Int, Int) -> SittingArea -> Pos -> Seat
updateAtPos (m, n) seats p =
  case seat of
    Empty ->
      if nSurOccupied == 0
        then Occupied
        else seat
    Occupied ->
      if nSurOccupied >= 5
        then Empty
        else seat
    _ -> Floor
  where
    seat = atPos (m, n) seats p
    -- surrounding = getSurrounding (m, n) seats p
    surrounding = getVisible seats p
    nSurOccupied = length (filter (== Occupied) surrounding)

getVisible :: SittingArea -> Pos -> [Seat]
getVisible seats p = map (\dir -> getFromDirection seats coord dir) directions
  where
    (m, n) = getSize seats
    coord = divMod p n
    incr = [(-1), 0, 1]
    directions = [(i, j) | i <- incr, j <- incr, (i, j) /= (0, 0)]

getFromDirection :: SittingArea -> Coord -> Direction -> Seat
getFromDirection seats (i, j) (di, dj)
  | i < 0 || j < 0 || i >= m || j >= n = Floor
  | i' < 0 || j' < 0 || i' >= m || j' >= n = Floor
  | otherwise =
    case nextSeat of
      Floor -> getFromDirection seats (i', j') (di, dj)
      _ -> nextSeat
  where
    (m, n) = getSize seats
    (i', j') = (i + di, j + dj)
    nextSeat = atCoord seats (i', j')

getSurrounding :: (Int, Int) -> SittingArea -> Pos -> [Seat]
getSurrounding (m, n) seats p = map (\c -> atCoord seats c) surCor
  where
    (i, j) = divMod p n
    incr = [(-1), 0, 1]
    surCor = [((i + i'), (j + j')) | i' <- incr, j' <- incr, (i', j') /= (0, 0)]

atPos :: (Int, Int) -> SittingArea -> Pos -> Seat
atPos (m, n) seats p
  | i < 0 || j < 0 || i >= m || j >= n = Floor
  | otherwise = (seats !! i) !! j
  where
    (i, j) = divMod p n

atCoord :: SittingArea -> Coord -> Seat
atCoord seats (i, j)
  | i < 0 || j < 0 || i >= m || j >= n = Floor
  | otherwise = (seats !! i) !! j
  where
    (m, n) = getSize seats

getSize :: SittingArea -> (Int, Int)
getSize m = ((length m), (length (m !! 0)))

readMap :: [String] -> SittingArea
readMap [] = []
readMap (x:xs) = row : (readMap xs)
  where
    row = map readPosition x

readPosition :: Char -> Seat
readPosition c =
  case c of
    '.' -> Floor
    '#' -> Occupied
    'L' -> Empty
