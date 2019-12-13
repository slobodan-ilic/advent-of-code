module Lib
  ( getNextCoordinate
  , getIntersection
  , getCoordinates
  , getIntersections
  , getSegments
  ) where

type Coordinate = (Int, Int)

type Coordinates = [Coordinate]

type Instruction = String

type Instructions = [Instruction]

type Segment = (Coordinate, Coordinate)

type Segments = [Segment]

getIntersection :: Segment -> Segment -> Maybe Coordinate
getIntersection a b =
  if parallel
    then Nothing
    else intersection
  where
    ((x1a, y1a), (x2a, y2a)) = a
    ((x1b, y1b), (x2b, y2b)) = b
    aTb = (x1a == x2a && y1b == y2b)
    bTa = (y1a == y2a && x1b == x2b)
    xaXxb = (x1a > x1b && x1a < x2b || x1a < x1b && x1a > x2b)
    yaXyb = (y1a > y1b && y1a < y2b || y1a < y1b && y1a > y2b)
    parallel = (x1a == x2a && x1b == x2b) || (y1a == y2a && y1b == y2b)
    intersection =
      if aTb && xaXxb
        then Just (x1a, y1b)
        else if bTa && yaXyb
               then Just (x1b, y1a)
               else Nothing

getNextCoordinate :: Coordinate -> Instruction -> Coordinate
getNextCoordinate coord instruction =
  case direction of
    'R' -> (x + distance, y)
    'L' -> (x - distance, y)
    'U' -> (x, y + distance)
    'D' -> (x, y - distance)
  where
    (x, y) = coord
    (direction:distStr) = instruction
    distance = read distStr :: Int

updateCoords :: Coordinates -> Instruction -> Coordinates
updateCoords coords instruction = coords ++ [next]
  where
    next = getNextCoordinate (last coords) instruction

getCoordinates :: Coordinate -> Instructions -> Coordinates
getCoordinates start instructions = foldl updateCoords [start] instructions

getSegments :: Coordinates -> Segments
getSegments (_:[]) = []
getSegments (a:b:xs) = (a, b) : (getSegments (b : xs))

getIntersections :: Segment -> Segments -> Coordinates
getIntersections a (h:[]) = coordinates
  where
    intersection = getIntersection a h
    Just coordinate = intersection
    coordinates =
      if intersection == Nothing
        then []
        else coordinate : []
getIntersections a (h:xs) =
  if intersection == Nothing
    then coordinates
    else coord : coordinates
  where
    intersection = getIntersection a h
    Just coord = intersection
    coordinates = getIntersections a xs
