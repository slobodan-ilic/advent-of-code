module Lib where

import Data.Char
import qualified Data.List as DLST

type Pos = (Int, Int)

type Vec = (Pos, Pos)

type State = [Pos]

type States = [State]

type Vectors = [Vec]

data Direction
  = U
  | R
  | D
  | L
  | UR
  | UL
  | DR
  | DL
  deriving (Eq, Show)

type Moves = [Move]

data Move =
  Move Direction Int
  deriving (Show)

navigate :: Vectors -> Vec -> Moves -> Vectors
navigate acc _ [] = acc
navigate acc vec (m:ms) = navigate acc' vec' ms
  where
    path = move vec m
    acc' = acc ++ path
    vec' = last acc'

reprState :: State -> [String]
reprState s =
  [ [ let (x, y) = (-x0 + j, y0 - i)
       in posToChar (x, y) s
  | j <- [0 .. 26]
  ]
  | i <- [0 .. 20]
  ]
  where
    (x0, y0) = (15, 15)

posToChar :: Pos -> State -> Char
posToChar (x, y) state = c
  where
    c =
      case DLST.elemIndex (x, y) state of
        Nothing ->
          if (x, y) == (0, 0)
            then 's'
            else '.'
        Just i ->
          if i == 0
            then 'H'
            else chr (i + ord '0')

navStates :: States -> Moves -> States
navStates acc [] = acc
navStates acc (m:mvs) = navStates acc' mvs
  where
    s = last acc
    path = moveState s m
    acc' = acc ++ path

move :: Vec -> Move -> Vectors
move vec (Move _ 0) = [vec]
move vec (Move dir nsteps) = vec' : move vec' (Move dir (nsteps - 1))
  where
    vec' = step vec dir

moveState :: State -> Move -> States
moveState s (Move _ 0) = []
moveState s (Move dir nsteps) = s' : moveState s' (Move dir (nsteps - 1))
  where
    s' = stepState s dir

stepState :: State -> Direction -> State
stepState [] _ = []
stepState (h:rest) d = correctState (h' : rest)
  where
    h' = moveHead h d

correctState :: State -> State
correctState [] = []
correctState (h:[]) = [h]
correctState (h:t:rest) =
  h :
  (if t' /= t
     then correctState (t' : rest)
     else t : rest)
  where
    (_, t') = correct' (h, t)

moveHead :: Pos -> Direction -> Pos
moveHead (x, y) d = (x + dx, y + dy)
  where
    (dx, dy) =
      case d of
        U -> (0, 1)
        R -> (1, 0)
        D -> (0, -1)
        L -> (-1, 0)

step :: Vec -> Direction -> Vec
step ((xh, yh), (xt, yt)) d = correct' ((xh', yh'), (xt, yt))
  where
    (xh', yh') = moveHead (xh, yh) d

correct' :: Vec -> Vec
correct' ((xh, yh), (xt, yt)) = ((xh, yh), (xt', yt'))
  where
    vx = xh - xt
    vy = yh - yt
    ax = abs vx
    ay = abs vy
    isDiag = ax + ay > 2
    dx =
      if isDiag
        then signum (xh - xt)
        else signum vx * (ax - 1)
    dy =
      if isDiag
        then signum (yh - yt)
        else signum vy * (ay - 1)
    xt' = xt + dx
    yt' = yt + dy

moves :: [String] -> [Move]
moves [] = []
moves (l:ls) = current : moves ls
  where
    direction =
      case head l of
        'R' -> R
        'L' -> L
        'U' -> U
        'D' -> D
    steps = read $ drop 2 l :: Int
    current = Move direction steps
