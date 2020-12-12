module Lib
  ( getActions
  , initState
  , execute
  ) where

type Unit = Int

type Degrees = Int

data CardinalDirection
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

data Direction
  = F
  | L
  | R
  deriving (Show, Eq)

data Action
  = Move Direction Unit
  | Slide CardinalDirection Unit
  | Turn Direction Degrees
  deriving (Show)

data State =
  State Waypoint CardinalDirection Unit Unit
  deriving (Show)

type Waypoint = (Unit, Unit)

turn :: State -> Action -> State
turn (State (dx, dy) cd x y) (Turn direction units) =
  if unitsLeft == 0
    then newState
    else turn newState newAction
  where
    unitsLeft = units - 90
    (dx', dy') =
      case direction of
        L -> (dy, (-dx))
        R -> ((-dy), dx)
    newState = State (dx', dy') cd x y
    newAction = Turn direction unitsLeft

move :: State -> Action -> State
move (State (dx, dy) cd x y) (Move F units) = State (dx, dy) cd x' y'
  where
    (x', y') = (x + units * dx, y + units * dy)

changeWaypoint :: State -> Action -> State
changeWaypoint (State (dx, dy) cd' x y) (Slide cd units) =
  State (dx', dy') cd' x y
  where
    (dx', dy') =
      case cd of
        North -> (dx + units, dy)
        South -> (dx - units, dy)
        East -> (dx, dy + units)
        West -> (dx, dy - units)

execute :: State -> [Action] -> State
execute state [] = state
execute state (a:as) = execute (performAction state a) as

initState :: State
initState = State (1, 10) East 0 0

performAction :: State -> Action -> State
performAction state action =
  case action of
    (Turn _ _) -> turn state action
    (Slide _ _) -> changeWaypoint state action
    _ -> move state action

getActions :: String -> [Action]
getActions contents = map parseAction $ lines contents

parseAction :: String -> Action
parseAction actStr =
  case h of
    'F' -> Move F num
    'L' -> Turn L num
    'R' -> Turn R num
    'S' -> Slide South num
    'N' -> Slide North num
    'W' -> Slide West num
    'E' -> Slide East num
  where
    h = head actStr
    num = read (tail actStr) :: Int
