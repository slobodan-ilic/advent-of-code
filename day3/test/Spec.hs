--test/Spec.hs
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Test day3 for advent of code"
      [ testGetIntersection
      , testGetCoordinates
      , testGetSegments
      , testGetIntersections
      , testContainsCoordinate
      , testGetMinDistance
      , testShortenSegment
      , testDistance
      ]

testGetIntersection :: TestTree
testGetIntersection =
  testCase "Testing getting intersection of two segments" $ do
    assertEqual "Orthogonal but oesn't intersect" Nothing $
      getIntersection ((2, 0), (3, 0)) ((-1, 1), (1, 1))
    assertEqual "Parallel doesn't intersect" Nothing $
      getIntersection ((2, 0), (3, 0)) ((2, 1), (3, 1))
    assertEqual "Doesn't intersect at (0, 0)" Nothing $
      getIntersection ((1, 0), (-1, 0)) ((0, 1), (0, -1))
    assertEqual "Doesn't intersect" Nothing $
      getIntersection ((0, 0), (75, 0)) ((155, 46), (155, -12))

testGetCoordinates :: TestTree
testGetCoordinates =
  testCase "Test getting coordinates from instructions" $ do
    assertEqual "Creates list for a single coordinate" [(0, 0), (992, 0)] $
      getCoordinates (0, 0) ["R992"]
    assertEqual
      "Creates list for multiple"
      [(0, 0), (992, 0), (992, 33), (970, 33), (970, 20)] $
      getCoordinates (0, 0) ["R992", "U33", "L22", "D13"]

testGetSegments :: TestTree
testGetSegments =
  testCase "Test getting segments from coordinates" $ do
    assertEqual "Creates single segment from two coordinates" [((0, 0), (0, 3))] $
      getSegments [(0, 0), (0, 3)]
    assertEqual
      "Creates two segments from three coordinates"
      [((0, 0), (0, 3)), ((0, 3), (3, 4))] $
      getSegments [(0, 0), (0, 3), (3, 4)]

testGetIntersections :: TestTree
testGetIntersections =
  testCase "Test getting intersections" $ do
    assertEqual "Intersects between target and one source" [(1, 0)] $
      getIntersections ((0, 0), (3, 0)) [((1, -1), (1, 1))]
    assertEqual
      "Intersects between target and multiple sources"
      [(2, 0), (1, 0)] $
      getIntersections
        ((0, 0), (3, 0))
        [((1, -1), (1, -2)), ((2, -1), (2, 3)), ((1, -1), (1, 1))]

testGetMinDistance :: TestTree
testGetMinDistance =
  testCase "Test getting min distance" $ do
    assertEqual "First simple example" 159 $
      getMinDistance
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"

testContainsCoordinate :: TestTree
testContainsCoordinate =
  testCase "Test if segment contains coordinate" $ do
    assertEqual "Test horizontal contains" True $
      containsCoordinate ((0, 3), (0, 10)) (0, 5)
    assertEqual "Test horizontal negative contains" True $
      containsCoordinate ((0, -3), (0, -10)) (0, -5)
    assertEqual "Test horizontal doesn't contain" False $
      containsCoordinate ((0, 3), (0, 10)) (0, 12)
    assertEqual "Test vertical contains" True $
      containsCoordinate ((3, 3), (10, 3)) (5, 3)
    assertEqual "Test vertical negative contains" True $
      containsCoordinate ((-3, -3), (-10, -3)) (-5, -3)
    assertEqual "Test vertical doesn't contain" False $
      containsCoordinate ((3, 3), (10, 3)) (12, 3)

testShortenSegment :: TestTree
testShortenSegment =
  testCase "Test shortening segment" $ do
    assertEqual "Test horizontal shortens" ((0, 3), (0, 5)) $
      shortenSegment ((0, 3), (0, 10)) (0, 5)
    assertEqual "Test horizontal doesn't shorten" ((0, 3), (0, 10)) $
      shortenSegment ((0, 3), (0, 10)) (0, 12)
    assertEqual "Test vertical shortens" ((3, 3), (5, 3)) $
      shortenSegment ((3, 3), (10, 3)) (5, 3)
    assertEqual "Test vertical doesn't shorten" ((3, 3), (10, 3)) $
      shortenSegment ((3, 3), (10, 3)) (12, 3)

testDistance :: TestTree
testDistance =
  testCase "Test distance" $ do
    assertEqual "Test distance 1" 22 $
      getDistanceUntil [((20, 3), (0, 3)), ((0, 3), (0, 10))] (0, 5)
    assertEqual "Test distance 2" 7 $
      getDistanceUntil [((0, 3), (0, 10))] (0, 12)
