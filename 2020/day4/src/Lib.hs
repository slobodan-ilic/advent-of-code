module Lib
  ( nValid
  , tokenize
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

getEntry :: [String] -> PassportEntry
getEntry entries =
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

getEntries :: PassportLines -> [PassportEntry]
getEntries lines = map getEntry $ tokenize lines

isFieldValid :: PassportEntry -> FieldType -> Bool
isFieldValid [] _ = False
isFieldValid ((Field ft strData):xs) fieldType =
  case ft == fieldType of
    True ->
      case fieldType of
        BYR -> isByrValid strData
        IYR -> isIyrValid strData
        EYR -> isEyrValid strData
        HGT -> isHgtValid strData
        HCL -> isHclValid strData
        ECL -> isEclValid strData
        PID -> isPidValid strData
        _ -> True
    _ -> isFieldValid xs fieldType

valid :: PassportEntry -> Bool
valid pe = all (isFieldValid pe) required

nValid :: PassportLines -> Int
nValid lines = sum $ map (\pe -> fromEnum $ valid pe) $ getEntries lines

isYearValid :: String -> Int -> Int -> Bool
isYearValid strData min max =
  case parsed of
    [(yr, _)] -> yr >= min && yr <= max
    _ -> False
  where
    parsed = reads strData :: [(Int, String)]

isByrValid :: String -> Bool
isByrValid strData = isYearValid strData 1920 2002

isIyrValid :: String -> Bool
isIyrValid strData = isYearValid strData 2010 2020

isEyrValid :: String -> Bool
isEyrValid strData = isYearValid strData 2020 2030

isHgtValid :: String -> Bool
isHgtValid strData =
  case parsed of
    [(hgt, unit)] ->
      case unit of
        "cm" -> hgt >= 150 && hgt <= 193
        "in" -> hgt >= 59 && hgt <= 76
        _ -> False
    _ -> False
  where
    parsed = reads strData :: [(Int, String)]

isHclValid :: String -> Bool
isHclValid (x:xs)
  | length xs /= 6 = False
  | x /= '#' = False
  | all (\el -> elem el ['a' .. 'f'] || elem el ['0' .. '9']) xs = True
  | otherwise = True

isEclValid :: String -> Bool
isEclValid strData =
  elem strData ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isPidValid :: String -> Bool
isPidValid strData =
  length strData == 9 && all (\d -> elem d ['0' .. '9']) strData
