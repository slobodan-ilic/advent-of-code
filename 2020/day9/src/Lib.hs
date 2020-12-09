module Lib
  ( process
  , numbers
  , process'
  , isValid
  , getOperands
  , getWeakCode
  ) where

type Pos = Int

type Preamble = [Int]

type PreambleLength = Int

type Numbers = [Int]

data Result
  = Success
  | Error Int
  deriving (Show)

getWeakCode :: Numbers -> Int -> Maybe Int
getWeakCode numbers target =
  case getOperands numbers target of
    Nothing -> Nothing
    (Just numbers) -> Just (minimum numbers + maximum numbers)

getOperands :: Numbers -> Int -> Maybe Numbers
getOperands numbers target = getOperands' 0 numbers target

getOperands' :: Int -> Numbers -> Int -> Maybe Numbers
getOperands' _ [] target = Nothing
getOperands' n numbers@(_:rest) target =
  if diff == 0
    then Just operands
    else if target - res < 0
           then getOperands' 0 rest target
           else getOperands' (n + 1) numbers target
  where
    operands = take n numbers
    res = sum operands
    diff = target - res

numbers :: [String] -> Numbers
numbers lines = map read lines

isValid :: Preamble -> Int -> Bool
isValid [] _ = False
isValid (p:ps) num =
  case elem diff ps of
    True -> True
    _ -> isValid ps num
  where
    diff = num - p

process :: PreambleLength -> Numbers -> Result
process n numbers = process' preamble rest
  where
    preamble = take n numbers
    rest = drop n numbers

process' :: Preamble -> Numbers -> Result
process' _ [] = Success
process' pa@(_:ps) (current:rest) =
  case isCurrentValid of
    True -> process' newPreamble rest
    False -> Error current
  where
    isCurrentValid = isValid pa current
    newPreamble = ps ++ [current]
