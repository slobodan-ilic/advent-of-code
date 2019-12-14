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
      , testGetMinDistance
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
