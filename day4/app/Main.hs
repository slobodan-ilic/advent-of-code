module Main where

import Lib

main :: IO ()
main = do
  print $ countPasswords (254032, 789860)
