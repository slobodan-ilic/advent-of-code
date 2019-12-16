module Lib
  ( areDigitsIncreasing
  , countPasswords
  ) where

type Range = (Int, Int)

areDigitsIncreasing :: Int -> Bool
areDigitsIncreasing 0 = True
areDigitsIncreasing n = ultimate >= penultimate && areDigitsIncreasing quotient
  where
    quotient = n `div` 10
    ultimate = n `mod` 10
    penultimate = quotient `mod` 10

hasConsecutive :: Int -> Bool
hasConsecutive 0 = False
hasConsecutive n =
  if ultimate == penultimate
    then True
    else hasConsecutive quotient
  where
    quotient = n `div` 10
    ultimate = n `mod` 10
    penultimate = quotient `mod` 10

countPasswords :: Range -> Int
countPasswords (a, b) =
  length $ [password | password <- (map isPassword candidates), password]
  where
    isPassword =
      \candidate -> areDigitsIncreasing candidate && hasConsecutive candidate
    candidates = [a .. b]
