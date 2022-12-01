module Lib
  ( getMax
  ) where

import Data.List

getMax :: [[String]] -> Int
getMax lists =
  foldr (+) 0 $
  take 3 $ sortBy (flip compare) $ map (\lst -> addItems lst) lists

addItems :: [String] -> Int
addItems items = foldr (+) 0 $ map (\ln -> read ln :: Int) items
