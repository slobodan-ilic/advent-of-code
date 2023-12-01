module Main
  ( main
  ) where

import qualified Data.List as DL
import qualified Data.List.Split as DS
import qualified Data.Map as DM
import Lib
import System.Environment

type Key = Int

type NInsp = Int

type Items = [Int]

type Operation = Int -> Int

type Test = Int

data Monkey =
  Monkey Key Items Operation Test (Key, Key) NInsp

instance Show Monkey where
  show (Monkey key items _ _ tgs ninsp) =
    "\n\nMonkey: " ++
    show key ++
    "\n  Items: " ++
    show items ++
    "\n  Throw to: " ++ show tgs ++ "\n  N of Insp: " ++ show ninsp

monkey :: [String] -> Monkey
monkey lns = Monkey key items op test (tgt1, tgt2) 0
  where
    key = read $ takeWhile (/= ':') $ drop 7 $ lns !! 0 :: Int
    items =
      read ("[" ++ (drop (length "  Starting items: ") $ lns !! 1) ++ "]") :: [Int]
    opChr = head $ drop (length "  Operation: new = old ") $ lns !! 2
    opArgStr = drop 2 $ drop (length "  Operation: new = old ") $ lns !! 2
    -- opArg = read opArgStr :: Int
    op =
      case opChr of
        '*' ->
          case opArgStr of
            "old" -> (^ 2)
            _ -> (* (read opArgStr :: Int))
        '+' ->
          case opArgStr of
            "old" -> (* 2)
            _ -> (+ (read opArgStr :: Int))
    test = read $ drop (length "  Test: divisible by ") $ lns !! 3 :: Int
    tgt1 =
      read $ drop (length "    If true: throw to monkey ") $ lns !! 4 :: Int
    tgt2 =
      read $ drop (length "    If false: throw to monkey ") $ lns !! 5 :: Int

turn :: DM.Map Int Monkey -> Int -> DM.Map Int Monkey
turn m k = nmp2
  where
    (Monkey key (it:its) op tst (t1, t2) ninsp) = m DM.! k
    -- nwl = div (op it) 3
    -- nwl = op it
    nwl = mod (op it) 9699690
    tgt =
      case mod nwl tst == 0 of
        True -> t1
        False -> t2
    (Monkey key' its' op' tst' (t1', t2') ninsp') = m DM.! tgt
    nm1 = Monkey key its op tst (t1, t2) (ninsp + 1)
    nm2 = Monkey key' (its' ++ [nwl]) op' tst' (t1', t2') ninsp'
    nmp1 = DM.insert key nm1 m
    nmp2 = DM.insert key' nm2 nmp1

turns :: DM.Map Int Monkey -> Int -> DM.Map Int Monkey
turns acc key =
  if length its == 0
    then acc
    else turns (turn acc key) key
  where
    (Monkey _ its _ _ _ _) = acc DM.! key

playRound :: DM.Map Int Monkey -> DM.Map Int Monkey
playRound acc = go acc [0 .. 7]
  where
    go acc [] = acc
    go acc (k:ks) = go acc' ks
      where
        acc' = turns acc k

main :: IO ()
main = do
  contents <- readFile "input.txt"
  args <- getArgs
  -- mapM_ putStrLn args
  let lns = lines contents
      monkeysLines = DS.splitOn [""] lns
      monkeys =
        DM.fromList
          [ (k, Monkey k is op tst (t1, t2) ninsp)
          | (Monkey k is op tst (t1, t2) ninsp) <- map monkey monkeysLines
          ]
      lstcm =
        foldr lcm 1 [tst | (_, (Monkey _ _ _ tst _ ninsp)) <- DM.toList monkeys]
      -- (Monkey k is op tst) = monkeys !! 0
      nRuns = read $ head $ tail args :: Int
      -- putStrLn $ show $ head $ tail args
      finState = iterate playRound monkeys !! nRuns
      elts = [ninsp | (_, (Monkey _ its _ _ _ ninsp)) <- DM.toList finState]
      sortedElts = DL.sort elts
   in putStrLn $ show $ sortedElts
   -- in putStrLn $ show $ (head $ reverse elts) * (head $ drop 1 $ reverse elts)
   -- in mapM_ (putStrLn . show) $ 
