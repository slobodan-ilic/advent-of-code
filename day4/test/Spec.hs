--test/Spec.hs
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Test day4 for advent of code"
      [testAreDigitsIncreasing, testHasConsecutive]

testAreDigitsIncreasing :: TestTree
testAreDigitsIncreasing =
  testCase "Test if digits are increasing" $ do
    assertEqual "One digit is increasing" True $ areDigitsIncreasing 3
    assertEqual "Two digits not decreasing" True $ areDigitsIncreasing 33
    assertEqual "Two digits decreasing" False $ areDigitsIncreasing 32
    assertEqual "Five digits decreasing" False $ areDigitsIncreasing 34256
    assertEqual "Five digits increasing" True $ areDigitsIncreasing 345689

testHasConsecutive :: TestTree
testHasConsecutive =
  testCase "Test has consecutive" $ do
    assertEqual "One digit is consecutive" True $ hasConsecutive 3
    assertEqual "Two digits are consecutive" True $ hasConsecutive 33
    assertEqual "Two digits not consecutive" False $ hasConsecutive 32
    assertEqual "Five digits not consecutive" False $ hasConsecutive 34256
    assertEqual "Five digits have consecutive" True $ hasConsecutive 345689
