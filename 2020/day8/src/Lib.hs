module Lib
  ( getOps
  , getOp
  , execOps
  , execOps'
  ) where

import Data.List.Split
import qualified Data.Set as Set

data Op
  = NoOp
  | Acc Int
  | Jmp Int
  deriving (Show, Eq)

type Ops = [Op]

type Res = Int

type Pos = Int

type Acc = Int

type Visited = Set.Set Int

execOps :: Ops -> Int
execOps ops = execOps' 0 Set.empty 0 ops

execOps' :: Pos -> Visited -> Acc -> Ops -> Int
execOps' pos visited acc ops =
  case Set.member pos visited of
    True -> acc
    _ -> execOps' nextPos newVisited newAcc ops
  where
    op = ops !! pos
    nextPos =
      case op of
        (Jmp offset) -> pos + offset
        _ -> pos + 1
    newAcc =
      case op of
        (Acc val) -> acc + val
        _ -> acc
    newVisited = Set.insert pos visited

getOps :: [String] -> Ops
getOps lines = [op | (Just op) <- filter (/= Nothing) $ map getOp lines]

getOp :: String -> Maybe Op
getOp line =
  case opStr of
    "acc" -> Just $ Acc param
    "jmp" -> Just $ Jmp param
    "nop" -> Just $ NoOp
    _ -> Nothing
  where
    [opStr, paramStr] = splitOn " " line
    param =
      case head paramStr of
        '+' -> read $ tail paramStr :: Int
        _ -> read paramStr :: Int
