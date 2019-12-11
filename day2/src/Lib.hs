module Lib
  ( intcode
  , getOps
  ) where

import Control.Monad.State
import Data.List.Split

type IntCodeState = [Int]

type IntCodeVal = [Int]

intcode :: Int -> State IntCodeState IntCodeVal
intcode (-1) = do
  xs <- get
  return xs
intcode i = do
  xs <- get
  let op:iop1:iop2:ires:_ = drop i xs
   in do put $ update (op, iop1, iop2, ires) xs
         intcode $ getNextPos i xs

getNextPos :: Int -> [Int] -> Int
getNextPos i xs =
  if i >= length xs || op == 99
    then -1
    else i + 4
  where
    op = head $ drop i xs

update :: (Int, Int, Int, Int) -> [Int] -> [Int]
update (op, iop1, iop2, ires) xs =
  case op of
    1 -> (y ++ [(xs !! iop1) + (xs !! iop2)] ++ ys)
      where (y, _:ys) = splitAt ires xs
    2 -> (y ++ [(xs !! iop1) * (xs !! iop2)] ++ ys)
      where (y, _:ys) = splitAt ires xs
    _ -> xs

getOps :: String -> [Int]
getOps contents = map ops entries
  where
    ops = \entry -> (read entry) :: Int
    entries = splitOn "," contents
