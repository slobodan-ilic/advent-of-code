module Main where

import Data.List
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let instructions = map getInstructions $ lines contents
  let coords = map (getCoordinates (0, 0)) instructions
  let segments = map getSegments coords
  let wire1:wire2:[] = segments
  let intersections = foldr (++) [] [getIntersections seg wire2 | seg <- wire1]
  let distances1 =
        map (\coordinate -> getDistanceUntil wire1 coordinate) intersections
  let distances2 =
        map (\coordinate -> getDistanceUntil wire2 coordinate) intersections
  print $ head $ sort [d1 + d2 | (d1, d2) <- zip distances1 distances2]
  print distances1
  print distances2
