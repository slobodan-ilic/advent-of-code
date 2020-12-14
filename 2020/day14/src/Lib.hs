module Lib
  ( parseInput
  , run
  , calculate
  , expandLoc
  , binarize
  , applyMaskToLoc
  , getLocations
  ) where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Map as Map
import Numeric (readInt, showIntAtBase)

type Location = Int

type Value = Int

type Bits = String

data Instruction
  = Mask Bits
  | Write Location Bits
  deriving (Show)

type Memory = Map.Map Int Int

data Comp =
  Comp Instruction Memory
  deriving (Show)

calculate :: Comp -> Int
calculate (Comp _ map) = sum map

run :: [Instruction] -> Comp
run insts = run' (Comp (Mask bits) map) insts
  where
    bits = replicate 36 'X'
    map = Map.empty

run' :: Comp -> [Instruction] -> Comp
run' comp [] = comp
run' comp (inst:rest) = run' newComp rest
  where
    newComp = executeInstruction comp inst

executeInstruction :: Comp -> Instruction -> Comp
executeInstruction comp@(Comp mask@(Mask bits) memory) inst =
  case inst of
    (Mask _) -> Comp inst memory
    (Write loc val) -> writeToLoc comp loc val'
      where val' = fst $ (readInt 2 (`elem` "01") digitToInt val) !! 0
    -- (Write loc val) -> Comp mask (Map.insert loc val' memory)
    --   where bits' = applyMask bits val
    --         val' = fst $ (readInt 2 (`elem` "01") digitToInt bits') !! 0

writeToLoc :: Comp -> Location -> Value -> Comp
writeToLoc comp@(Comp (Mask mask) memory) loc val =
  writeToLocations comp locs val
  where
    locs = getLocations mask loc

writeToLocations :: Comp -> [Bits] -> Value -> Comp
writeToLocations comp [] _ = comp
writeToLocations comp@(Comp mask memory) (b:bs) val =
  writeToLocations comp' bs val
  where
    loc = fst $ (readInt 2 (`elem` "01") digitToInt b) !! 0
    memory' = Map.insert loc val memory
    comp' = Comp mask memory'

getLocations :: Bits -> Location -> [Bits]
getLocations mask loc = expandLoc newLocBits
  where
    binLoc = binarize 36 loc
    newLocBits = applyMaskToLoc mask binLoc

expandLoc :: Bits -> [Bits]
expandLoc bits = map (\comb -> materializeLoc bits comb) combs
  where
    n = length $ filter (== 'X') bits
    combs = map (\comb -> binarize n comb) [0 .. (2 ^ n) - 1]

materializeLoc :: Bits -> Bits -> Bits
materializeLoc bits [] = bits
materializeLoc bits (c:cs) = first ++ [c] ++ (materializeLoc second cs)
  where
    first = takeWhile (/= 'X') bits
    second = drop 1 (dropWhile (/= 'X') bits)

binarize :: Int -> Int -> String
binarize len val = zeros ++ bits
  where
    bits = showIntAtBase 2 intToDigit val ""
    zeros = replicate (len - length bits) '0'

applyMaskToLoc :: Bits -> Bits -> Bits
applyMaskToLoc mask val =
  [ if m == '0'
    then v
    else m
  | (m, v) <- zip mask val
  ]

applyMask :: Bits -> Bits -> Bits
applyMask mask val =
  [ if m == 'X'
    then v
    else m
  | (m, v) <- zip mask val
  ]

parseInput :: String -> [Instruction]
parseInput contents = map parseInstruction lns
  where
    lns = lines contents

parseInstruction :: String -> Instruction
parseInstruction inst
  | take 7 inst == "mask = " = Mask $ drop 7 inst
  | otherwise = Write loc (zeros ++ bits)
  where
    loc = read (takeWhile (/= ']') (drop 4 inst)) :: Int
    val = read (drop 2 (dropWhile (/= '=') inst)) :: Int
    bits = showIntAtBase 2 intToDigit val ""
    zeros = replicate (36 - length bits) '0'
