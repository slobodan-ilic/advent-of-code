module Lib
  ( processRule
  , getTotalBags
  ) where

import Data.List.Split

data Rule =
  Rule Key BagContents
  deriving (Show)

data Entry =
  Entry Key Count
  deriving (Show)

data BagContents =
  BagContents (Maybe [Entry])
  deriving (Show)

type Count = Int

type Key = String

type Rules = [Rule]

-- Part 2
-- ---------------------------------
getTotalBags :: Rules -> Key -> Count
getTotalBags rules key =
  case contents of
    Nothing -> 0
    Just entries ->
      sum
        [ count + count * (getTotalBags rules key')
        | (Entry key' count) <- entries
        ]
  where
    (Rule _ (BagContents contents)) = findRule rules key

findRule :: Rules -> Key -> Rule
findRule rules key = head $ dropWhile (\(Rule key' _) -> key' /= key) rules

-- Part 1
-- ---------------------------------
getPaths :: Rules -> [Key] -> [Key]
getPaths rules keys = getPaths' [] rules keys

getPaths' :: [Key] -> Rules -> [Key] -> [Key]
getPaths' acc _ [] = acc
getPaths' acc rules keys = getPaths' (acc ++ new) rules containers
  where
    containers = getContainers rules keys
    new = filter (\el -> not (elem el acc)) containers

getContainers :: Rules -> [Key] -> [Key]
getContainers rules keys = getContainers' [] rules keys

getContainers' :: [Key] -> Rules -> [Key] -> [Key]
getContainers' acc _ [] = acc
getContainers' acc rules (k:keys) = getContainers' (acc ++ new) rules keys
  where
    containers = getContainersForKey rules k
    new = filter (\el -> not (elem el acc)) containers

getContainersForKey :: Rules -> Key -> [Key]
getContainersForKey [] _ = []
getContainersForKey (rule@(Rule key' _):rules) key =
  case hasKey rule key of
    True -> key' : (getContainersForKey rules key)
    _ -> getContainersForKey rules key

hasKey :: Rule -> Key -> Bool
hasKey (Rule _ (BagContents Nothing)) _ = False
hasKey (Rule _ (BagContents (Just entries))) key =
  any (== key) [k | (Entry k _) <- entries]

processRule :: String -> Rule
processRule line = Rule key contents
  where
    [keyPart, contentsPart] = splitOn "contain" line
    key = getKey keyPart
    contents = getBagContents contentsPart

getKey :: String -> Key
getKey line = concat $ take 2 $ words line

getBagContents :: String -> BagContents
getBagContents line =
  if line == " no other bags."
    then BagContents Nothing
    else BagContents (Just entries)
  where
    tokens = splitOn "," line
    entries = map getEntry tokens

getEntry :: String -> Entry
getEntry line = Entry key count
  where
    tokens = words line
    count = read (tokens !! 0) :: Int
    key = concat $ take 2 $ drop 1 $ words line
