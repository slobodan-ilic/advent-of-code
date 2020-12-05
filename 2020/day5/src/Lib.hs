module Lib
  ( findMySeat
  , getSeatId
  ) where

getIntFromBinStr :: (Char, Char) -> String -> Int
getIntFromBinStr _ [] = 0
getIntFromBinStr (zeroKey, oneKey) (x:xs) =
  round (base * 2 ** exp) + getIntFromBinStr (zeroKey, oneKey) xs
  where
    base =
      if x == oneKey
        then 1
        else if x == zeroKey
               then 0
               else 0 -- shouldn't reach
    exp = fromIntegral $ length xs

getRowId :: String -> Int
getRowId = getIntFromBinStr ('F', 'B')

getColId :: String -> Int
getColId = getIntFromBinStr ('L', 'R')

getSeatId :: String -> Int
getSeatId x = rowId * 8 + colId
  where
    rowId = getRowId $ take 7 x
    colId = getColId $ drop 7 x

findMySeat :: [Int] -> Maybe Int
findMySeat [] = Nothing
findMySeat (_:[]) = Nothing
findMySeat (x:y:xs) =
  if y - x == 2
    then Just (y - 1)
    else findMySeat (y : xs)
