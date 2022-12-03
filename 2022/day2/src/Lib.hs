module Lib
  ( process
  , moves
  , roundScore
  ) where

data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq)

instance Ord Shape where
  compare x y =
    case x == y of
      True -> EQ
      False ->
        if y == xVictor
          then LT
          else GT
    where
      lst = [Rock, Paper, Scissors]
      buffer = zip (cycle lst) (drop 1 $ cycle lst)
      xVictor = snd $ head $ dropWhile (\(x', _) -> x' /= x) buffer

type Round = (Shape, Shape)

type Game = [String]

data Result
  = Lose
  | Draw
  | Win

process :: Game -> Int
process [] = 0
process (x:xs) = (roundScore $ moves x) + (process xs)

roundScore :: Round -> Int
roundScore (op, me) = shapeVal + result
  where
    (shapeVal, result) =
      case me of
        Rock ->
          ( 1
          , case op of
              Rock -> 3
              Paper -> 0
              Scissors -> 6)
        Paper ->
          ( 2
          , case op of
              Rock -> 6
              Paper -> 3
              Scissors -> 0)
        Scissors ->
          ( 3
          , case op of
              Rock -> 0
              Paper -> 6
              Scissors -> 3)

moves :: String -> Round
moves (a:' ':c:rest) = (opMove, meMove)
  where
    opMove =
      case a of
        'A' -> Rock
        'B' -> Paper
        'C' -> Scissors
    needOrd =
      case c of
        'X' -> LT
        'Y' -> EQ
        'Z' -> GT
    lst = [Rock, Paper, Scissors]
    meMove = head $ dropWhile (\x -> compare opMove x /= needOrd) lst
