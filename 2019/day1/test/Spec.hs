--test/Spec.hs
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain
    (testGroup
       "Test day2 for advent of code"
       [testGetsFuelSimple, testGetsFuelRecursive1, testGetsFuelRecursive2])

testGetsFuelSimple :: TestTree
testGetsFuelSimple =
  testCase
    "Testing getFuel simple"
    (assertEqual "Fuel for mass 12 should equal 2" 2 (getFuel 12))

testGetsFuelRecursive1 :: TestTree
testGetsFuelRecursive1 =
  testCase
    "Testing getFuel recursive case 1"
    (assertEqual "Fuel for mass 1969 should equal 966" 966 (getFuel 1969))

testGetsFuelRecursive2 :: TestTree
testGetsFuelRecursive2 =
  testCase
    "Testing getFuel recursive case 2"
    (assertEqual
       "Fuel for mass 100756 should equal 50346"
       50346
       (getFuel 100756))
