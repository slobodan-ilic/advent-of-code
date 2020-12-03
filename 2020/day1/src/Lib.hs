module Lib
  ( getProduct3
  ) where

getProduct3 :: [Int] -> Int
getProduct3 xs =
  [(x * y * z) | x <- xs, y <- xs, z <- xs, x + y + z == 2020] !! 0
