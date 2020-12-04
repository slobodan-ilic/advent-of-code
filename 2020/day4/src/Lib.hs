module Lib
  ( nValid
  , tokenize
  , hasField
  , entries
  , required
  ) where

import Data.List.Split

data FieldType
  = BYR
  | IYR
  | EYR
  | HGT
  | HCL
  | ECL
  | PID
  | CID
  deriving (Show, Eq)

data Field =
  Field FieldType String
  deriving (Show, Eq)

type PassportEntry = [Field]

type PassportLine = String

type PassportLines = [String]

type PassportTokens = [String]

required :: [FieldType]
required = [BYR, IYR, EYR, HGT, HCL, ECL, PID]

optional :: [FieldType]
optional = [CID]

field :: String -> Maybe Field
field entry =
  case ft of
    (Just fType) -> Just $ Field fType val
    _ -> Nothing
  where
    [key, val] = splitOn ":" entry
    ft =
      case key of
        "byr" -> Just BYR
        "iyr" -> Just IYR
        "eyr" -> Just EYR
        "hgt" -> Just HGT
        "hcl" -> Just HCL
        "ecl" -> Just ECL
        "pid" -> Just PID
        "cid" -> Just CID
        _ -> Nothing

entry :: [String] -> PassportEntry
entry entries =
  [field | (Just field) <- filter (/= Nothing) $ map field entries]

tokenize :: PassportLines -> [PassportTokens]
tokenize [] = []
tokenize lines = headEntry : tokenize rest
  where
    (headEntry, rest) = getNext [] lines

getNext :: PassportTokens -> PassportLines -> (PassportTokens, PassportLines)
getNext acc [] = (acc, [])
getNext acc (l:ls) =
  case l of
    "" -> (acc, ls)
    _ -> getNext (acc ++ (words l)) ls

entries :: PassportLines -> [PassportEntry]
entries lines = map entry $ tokenize lines

hasField :: PassportEntry -> FieldType -> Bool
hasField [] _ = False
hasField ((Field ft _):xs) fieldType =
  case ft == fieldType of
    True -> True
    _ -> hasField xs fieldType

valid :: PassportEntry -> Bool
valid pe = all (hasField pe) required

nValid :: PassportLines -> Int
nValid lines = sum $ map (\pe -> fromEnum $ valid pe) $ entries lines
