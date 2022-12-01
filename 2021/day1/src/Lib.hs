module Lib
  ( nInc
  ) where

nInc :: [Int] -> Int
nInc xs =
  sum
    (map
       (\d ->
          if d > 0
            then 1
            else 0)
       (zipWith (-) (tail xs) xs))
