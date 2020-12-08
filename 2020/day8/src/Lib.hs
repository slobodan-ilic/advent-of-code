module Lib
  ( getOps
  , fixProgram
  ) where

import Data.List.Split
import qualified Data.Set as Set

data Op
  = NoOp Int
  | Acc Int
  | Jmp Int
  deriving (Show, Eq)

data Code
  = Failed Acc
  | Success Acc
  deriving (Show)

type Ops = [Op]

type Res = Int

type Pos = Int

type Acc = Int

type Visited = Set.Set Int

fixProgram :: Ops -> Int
fixProgram ops = fixProgram' 0 ops

fixProgram' :: Pos -> Ops -> Int
fixProgram' pos ops =
  case res of
    Success acc -> acc
    _ -> fixProgram' (pos + 1) ops
  where
    res = execOps $ changeProgram ops pos

changeProgram :: Ops -> Pos -> Ops
changeProgram ops pos = take pos ops ++ [newOp] ++ drop (pos + 1) ops
  where
    op = ops !! pos
    newOp =
      case op of
        (NoOp val) -> Jmp val
        (Jmp val) -> NoOp val
        _ -> op

execOps :: Ops -> Code
execOps ops = execOps' 0 Set.empty 0 ops

execOps' :: Pos -> Visited -> Acc -> Ops -> Code
execOps' pos visited acc ops =
  case Set.member pos visited of
    True -> Failed acc
    _ ->
      case pos == length ops of
        True -> Success acc
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
    "nop" -> Just $ NoOp param
    _ -> Nothing
  where
    [opStr, paramStr] = splitOn " " line
    param =
      case head paramStr of
        '+' -> read $ tail paramStr :: Int
        _ -> read paramStr :: Int
