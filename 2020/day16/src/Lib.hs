module Lib where

import Data.List
import Data.List.Split

type Field = Int

type Ticket = [Field]

data Range =
  Range Int Int
  deriving (Show)

data Validator =
  Validator String [Range]
  deriving (Show)

run :: String -> [(Int, [Int], Int)]
run contents = whichInd
  where
    (vs, ts, t) = parseInput contents
    vts = filter (\t -> isTicketValid vs t) ts
    fcls = fieldCols vts
    which =
      map (\(fcol, i) -> (i, whichValidatorApplies vs fcol)) (zip fcls [0 ..])
    whichInd =
      sort $
      map
        (\(i', w) ->
           let lst = [i | (i, x) <- zip [0 ..] w, x == True]
            in (length lst, lst, i'))
        which

whichValidatorApplies :: [Validator] -> [Field] -> [Bool]
whichValidatorApplies vs fcol = map (\v -> doesValidatorApply v fcol) vs

fieldCols :: [Ticket] -> [[Field]]
fieldCols [] = []
fieldCols ts
  | ts !! 0 == [] = []
  | otherwise = hs : fieldCols tls
  where
    (hs, tls) = headTickets ts

headTickets :: [Ticket] -> ([Field], [Ticket])
headTickets ts = (heads, tails)
  where
    heads = map head ts
    tails = map tail ts

doesValidatorApply :: Validator -> [Field] -> Bool
doesValidatorApply v fs = all (\f -> isFieldValid v f) fs

isTicketValid :: [Validator] -> Ticket -> Bool
isTicketValid vs t = all (\f -> isValidForAny vs f) t

isValidForAny :: [Validator] -> Field -> Bool
isValidForAny vs f = any (\v -> isFieldValid v f) vs

isFieldValid :: Validator -> Field -> Bool
isFieldValid (Validator _ rs) f = any (\r -> inRange r f) rs

inRange :: Range -> Field -> Bool
inRange (Range min' max') f = f >= min' && f <= max'

parseInput :: String -> ([Validator], [Ticket], Ticket)
parseInput contents = (validators, tickets, myTicket)
  where
    lns = lines contents
    segments = splitOn [""] lns
    validators = map parseValidator (segments !! 0)
    tickets = map parseTicket (tail $ segments !! 2)
    myTicket = parseTicket ((segments !! 1) !! 1)

parseTicket :: String -> Ticket
parseTicket s = fields
  where
    fieldsStr = splitOn "," s
    fields = map (\f -> read f :: Int) fieldsStr

parseValidator :: String -> Validator
parseValidator s = Validator name ranges
  where
    name = takeWhile (/= ':') s
    strRanges = splitOn " or " (drop 2 (dropWhile (/= ':') s))
    ranges = map parseRange strRanges

parseRange :: String -> Range
parseRange s = Range min' max'
  where
    nums = splitOn "-" s
    min' = read (nums !! 0) :: Int
    max' = read (nums !! 1) :: Int
