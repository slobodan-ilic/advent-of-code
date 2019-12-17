module Lib
  ( areDigitsIncreasing
  , countPasswords
  , getPasswords
  , getDigits
  , hasConsecutive
  , isPassword
  , streaks
  ) where

import Data.List

type Range = (Int, Int)

areDigitsIncreasing :: Int -> Bool
areDigitsIncreasing 0 = True
areDigitsIncreasing n = ultimate >= penultimate && areDigitsIncreasing quotient
  where
    quotient = n `div` 10
    ultimate = n `mod` 10
    penultimate = quotient `mod` 10

hasConsecutive :: Int -> Bool
hasConsecutive 0 = True
hasConsecutive n =
  if ultimate == penultimate
    then True
    else hasConsecutive quotient
  where
    quotient = n `div` 10
    ultimate = n `mod` 10
    penultimate = quotient `mod` 10

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits n = (getDigits quotient) ++ [n `mod` 10]
  where
    quotient = n `div` 10

streaks :: [Int] -> [Int]
streaks [] = []
streaks (h:xs) = (length match) : streaks rest
  where
    (match, rest) = span (\x -> x == h) (h : xs)

countPasswords :: Range -> Int
countPasswords (a, b) = length $ filter isPassword candidates
  where
    candidates = [a .. b]

getPasswords :: Range -> [Int]
getPasswords (a, b) = filter isPassword candidates
  where
    candidates = [a .. b]

isPassword :: Int -> Bool
isPassword candidate =
  areDigitsIncreasing candidate && elem 2 (streaks (getDigits candidate))
