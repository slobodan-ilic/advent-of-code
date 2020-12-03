--test/Spec.hs
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain (testGroup "Test day2 for advent of code" [])
-- testAddOp :: TestTree
-- testAddOp =
--   testCase
--     "Testing addition operation"
--     (assertEqual
--        "Should update position 0 to 2"
--        [2, 0, 0, 0, 99]
--        (intcode [1, 0, 0, 0, 99]))
-- testGetSliceAt0 :: TestTree
-- testGetSliceAt0 =
--   testCase
--     "Test getting 0th slice"
--     (assertEqual
--        "Should get first 4 elements"
--        (1, 0, 0, 0)
--        (getSliceAt 0 [1, 0, 0, 0, 99]))
-- testGetSliceAtN :: TestTree
-- testGetSliceAtN =
--   testCase
--     "Test getting nth slice"
--     (assertEqual
--        "Should get last 4 elements"
--        (99, 2, 3, 4)
--        (getSliceAt 1 [1, 0, 0, 0, 99, 2, 3, 4]))
-- testProcessSliceAt0 :: TestTree
-- testProcessSliceAt0 =
--   testCase
--     "Test processing slice at 0"
--     (assertEqual
--        "Should get updated list"
--        [0, 0, 0, 0, 99, 2, 3, 4]
--        (processSliceAt 0 [1, 0, 0, 0, 99, 2, 3, 4]))
-- testProcessSliceAt1 :: TestTree
-- testProcessSliceAt1 =
--   testCase
--     "Test processing slice at 1"
--     (assertEqual
--        "Should get updated list"
--        [1, 0, 0, 0, 6, 2, 3, 4]
--        (processSliceAt 1 [1, 0, 0, 0, 2, 2, 3, 4]))
