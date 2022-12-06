module Lib
  ( fillCargo
  , fillLine
  , processCommand
  , command
  , processCargo
  ) where

type Line = String

type Lines = [String]

type Crate = Char

type Stack = [Crate]

type Cargo = [Stack]

type StackInds = [(Char, Int)]

type Count = Int

type Source = Int

type Dest = Int

data Command =
  Command Count Source Dest
  deriving (Show)

-- processCargo :: Lines -> Cargo
-- processCargo x = cargo'
processCargo :: Lines -> String
processCargo x = [head $ reverse stack | stack <- cargo']
  where
    cargo = fillCargo $ takeWhile (/= "") x
    -- commands = take 2 $ tail $ dropWhile (/= "") x
    commands = tail $ dropWhile (/= "") x
    cargo' = process cargo commands

process :: Cargo -> Lines -> Cargo
process acc [] = acc
process acc (x:xs) = process (processCommand acc $ command x) xs

-- process acc _ = acc
processCommand :: Cargo -> Command -> Cargo
processCommand acc (Command cnt src dst) = acc'
  where
    src' = src - 1
    dst' = dst - 1
    srcStack = acc !! src'
    dstStack = acc !! dst'
    load = take cnt $ reverse srcStack
    n = length acc
    srcStack' = take (length srcStack - cnt) srcStack
    dstStack' = concat [dstStack, reverse load]
    acc' =
      [ if i == src'
        then srcStack'
        else if i == dst'
               then dstStack'
               else acc !! i
      | i <- [0 .. (n - 1)]
      ]

command :: Line -> Command
command line = Command count source dest
  where
    tokens = words line
    count = read $ tokens !! 1 :: Int
    source = read $ tokens !! 3 :: Int
    dest = read $ tokens !! 5 :: Int

fillCargo :: Lines -> Cargo
fillCargo x = addCargo (take n $ repeat "") stacksInds crates
  where
    cargo = takeWhile (/= "") x
    stacks = filter (/= ' ') $ head $ reverse cargo
    crates = tail $ reverse cargo
    stacksInds = zip stacks [1,5 ..]
    n = length stacksInds

addCargo :: Cargo -> StackInds -> Lines -> Cargo
addCargo acc _ [] = acc
addCargo acc stackInds (x:xs) = addCargo (addLine acc stackInds x) stackInds xs

fillLine :: StackInds -> Line -> Cargo
fillLine stackInds line =
  [ if (line !! ind) /= ' '
    then [line !! ind]
    else []
  | (_, ind) <- stackInds
  ]

addLine :: Cargo -> StackInds -> Line -> Cargo
addLine acc stackInds [] = acc
addLine acc stackInds line = [concat [as, cs] | (as, cs) <- zip acc cargo]
  where
    cargo = fillLine stackInds line
