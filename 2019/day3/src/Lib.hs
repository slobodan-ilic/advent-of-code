module Lib
  ( containsCoordinate
  , distance
  , getCoordinates
  , getDistanceUntil
  , getInstructions
  , getIntersectionsFromString
  , getIntersection
  , getIntersections
  , getMinDistance
  , getNextCoordinate
  , getSegments
  , getSegmentsUntil
  , shortenSegment
  ) where

import Data.List
import Data.List.Split

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
    xaXxb =
      (x1a > x1b && x1a < x2b || x1a < x1b && x1a > x2b) &&
      (y1b < y1a && y1b > y2a || y1b > y1a && y1b < y2a)
    yaXyb =
      (y1a > y1b && y1a < y2b || y1a < y1b && y1a > y2b) &&
      (x1b < x1a && x1b > x2a || x1b > x1a && x1b < x2a)
    parallel = (x1a == x2a && x1b == x2b) || (y1a == y2a && y1b == y2b)
    intersection =
      if aTb && xaXxb
        then if (x1a, y1b) == (0, 0)
               then Nothing
               else Just (x1a, y1b)
        else if bTa && yaXyb
               then if (x1b, y1a) == (0, 0)
                      then Nothing
                      else Just (x1b, y1a)
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

getInstructions :: String -> [String]
getInstructions contents = splitOn "," contents

getMinDistance :: String -> Int
getMinDistance contents = head $ sort distances
  where
    intersections = getIntersectionsFromString contents
    distances = map (\(x, y) -> abs x + abs y) intersections

getIntersectionsFromString :: String -> Coordinates
getIntersectionsFromString contents = intersections
  where
    instructions = map getInstructions $ lines contents
    coords = map (getCoordinates (0, 0)) instructions
    segments = map getSegments coords
    wire1:wire2:[] = segments
    intersections = foldr (++) [] [getIntersections seg wire2 | seg <- wire1]

containsCoordinate :: Segment -> Coordinate -> Bool
containsCoordinate ((x1, y1), (x2, y2)) (x, y) =
  x1 == x2 && x == x1 && (y < y1 && y > y2 || y > y1 && y < y2) ||
  y1 == y2 && y == y1 && (x < x1 && x > x2 || x > x1 && x < x2)

shortenSegment :: Segment -> Coordinate -> Segment
shortenSegment segment coordinate =
  if containsCoordinate segment coordinate
    then (a, coordinate)
    else segment
  where
    (a, _) = segment

getSegmentsUntil :: Segments -> Coordinate -> Segments
getSegmentsUntil segments coordinate =
  match ++
  if rest /= []
    then [shortenSegment (head rest) coordinate]
    else []
  where
    (match, rest) =
      span (\segment -> not $ containsCoordinate segment coordinate) segments

distance :: Segments -> Int
distance segments =
  foldl (+) 0 $
  map (\((x1, y1), (x2, y2)) -> abs (x1 - x2) + abs (y1 - y2)) segments

getDistanceUntil :: Segments -> Coordinate -> Int
getDistanceUntil segments coordinate =
  distance $ getSegmentsUntil segments coordinate
