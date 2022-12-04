module Lib
  ( assignments
  , containments
  , overlaps
  , part1
  , part2
  ) where

type Range = (Int, Int)

type Assignment = (Range, Range)

type Lines = [String]

part1 :: Lines -> Int
part1 lines = length [c | c <- containments lines, c == True]

part2 :: Lines -> Int
part2 lines = length [c | c <- overlaps lines, c == True]

assignments :: [String] -> [Assignment]
assignments [] = []
assignments (x:xs) = (range x) : (assignments xs)

containments :: Lines -> [Bool]
containments [] = []
containments (x:xs) = contained (range x) : (containments xs)

overlaps :: Lines -> [Bool]
overlaps [] = []
overlaps (x:xs) = overlap (range x) : (overlaps xs)

range :: String -> Assignment
range xs = ((a, b), (c, d))
  where
    part1 = takeWhile (/= ',') xs
    a = read $ takeWhile (/= '-') part1 :: Int
    b = read $ tail $ dropWhile (/= '-') part1 :: Int
    part2 = tail $ dropWhile (/= ',') xs
    c = read $ takeWhile (/= '-') part2 :: Int
    d = read $ tail $ dropWhile (/= '-') part2 :: Int

contained :: Assignment -> Bool
contained ((a, b), (c, d)) = a <= c && b >= d || c <= a && d >= b

overlap :: Assignment -> Bool
overlap ((a, b), (c, d)) = a < d && b >= c || c < b && d >= a
