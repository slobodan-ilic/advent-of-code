module Main
  ( main
  ) where

import Data.List.Split (splitOn)
import qualified Data.Map as M

type Game = String

type Games = [Game]

type Round = String

type Rounds = [Round]

isGamePossible :: Game -> Bool
isGamePossible game = all isRoundPossible rounds
  where
    roundsStr = (splitOn ":" game) !! 1
    rounds = splitOn ";" roundsStr

maxAllowedByColor :: M.Map String Int
maxAllowedByColor = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

isRoundPossible :: Round -> Bool
isRoundPossible round = all isCombinationPossible balls
  where
    balls = map (dropWhile (== ' ')) (splitOn "," round)

isCombinationPossible :: String -> Bool
isCombinationPossible combination =
  case colorMax of
    Nothing -> False
    Just n -> n >= (read numeral :: Int)
  where
    [numeral, color] = splitOn " " combination
    colorMax = M.lookup color maxAllowedByColor

gameId :: Game -> Int
gameId game = read $ gameIdStr
  where
    gameHeader = head $ splitOn ":" game
    gameIdStr = last $ splitOn " " $ gameHeader

processGamesPart1 :: Games -> [Int]
processGamesPart1 [] = []
processGamesPart1 (x:xs) =
  if isGamePossible x
    then gameId x : (processGamesPart1 xs)
    else processGamesPart1 xs

maxForColor :: Game -> String -> Int
maxForColor game color = maximum numbers
  where
    colorBits = splitOn color game
    trimmedReversed = map (\x -> dropWhile (== ' ') (reverse x)) colorBits
    numReversed = map (takeWhile (\y -> elem y ['0' .. '9'])) trimmedReversed
    numbers = [read (reverse x) :: Int | x <- numReversed, x /= ""]

gamePower :: Game -> Int
gamePower game = x * y * z
  where
    x = maxForColor game "red"
    y = maxForColor game "blue"
    z = maxForColor game "green"

processGamesPart2 :: Games -> [Int]
processGamesPart2 [] = []
processGamesPart2 (g:gs) = (gamePower g) : (processGamesPart2 gs)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ sum $ processGamesPart2 $ lines contents
