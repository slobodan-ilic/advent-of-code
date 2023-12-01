module Main
  ( main
  ) where

import qualified Data.List.Split as SP
import Lib

data Cmd
  = Noop
  | Addx Int
  deriving (Show)

type Reg = Int

data Cycle =
  Cycle Int Reg
  deriving (Show)

cmds :: [String] -> [Cmd]
cmds [] = []
cmds (l:ls) = cmd l : cmds ls

cmd :: String -> Cmd
cmd s
  | s == "noop" = Noop
  | otherwise = Addx (read (tail $ dropWhile (/= ' ') s) :: Int)

processCmd :: Cycle -> Cmd -> [Cycle]
processCmd (Cycle ord regX) cmd =
  case cmd of
    Noop -> [Cycle (ord + 1) regX]
    Addx a -> [Cycle (ord + 1) regX, Cycle (ord + 2) (regX + a)]

signal :: [Cycle] -> [Cmd] -> [Cycle]
signal acc [] = acc
signal acc (c:cs) = signal acc' cs
  where
    spl = last acc
    acc' = acc ++ processCmd spl c

data Pixel =
  Pixel Int Int Char
  deriving (Show)

screen :: [Cycle] -> [(Pixel, Cycle)]
screen sig =
  [ ( let i' = mod i 40
          j' = div i 40
          v =
            if i' >= regX - 1 && i' <= regX + 1
              then '#'
              else '.'
       in Pixel i' j' v
    , (Cycle ord regX))
  | (i, (Cycle ord regX)) <- zip [0 ..] sig
  ]

strengths :: [Int] -> Int -> [Cycle] -> [Int]
strengths acc i sps
  | i >= length sps = acc
  | otherwise = strengths acc' (i + 40) sps
  where
    (Cycle ord regX) = sps !! i
    acc' = acc ++ [ord * regX]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lns = lines contents
      cds = cmds lns
      sample1 = Cycle 1 1
      sig = signal [sample1] cds
      scr = takeWhile ((== 40) . length) $ SP.splitEvery 40 $ screen sig
      output = [[c | ((Pixel _ _ c), _) <- ln] | ln <- scr]
   in mapM_ (putStrLn . show) $ output
   -- in putStrLn $ show $ output
