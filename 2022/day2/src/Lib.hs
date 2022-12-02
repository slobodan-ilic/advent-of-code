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

type Round = (Shape, Shape)

type Game = [String]

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
moves (a:' ':c:rest) =
  case a of
    'A' ->
      ( Rock
      , case c of
          'X' -> Scissors
          'Y' -> Rock
          'Z' -> Paper)
    'B' ->
      ( Paper
      , case c of
          'X' -> Rock
          'Y' -> Paper
          'Z' -> Scissors)
    'C' ->
      ( Scissors
      , case c of
          'X' -> Paper
          'Y' -> Scissors
          'Z' -> Rock)
