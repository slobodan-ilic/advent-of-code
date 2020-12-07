module Lib
  ( processRule
  , getPaths
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

getPaths :: [Rule] -> [Key] -> [Key]
getPaths rules keys = getPaths' [] rules keys

getPaths' :: [Key] -> [Rule] -> [Key] -> [Key]
getPaths' acc _ [] = acc
getPaths' acc rules keys = getPaths' (acc ++ new) rules containers
  where
    containers = getContainers rules keys
    new = filter (\el -> not (elem el acc)) containers

getContainers :: [Rule] -> [Key] -> [Key]
getContainers rules keys = getContainers' [] rules keys

getContainers' :: [Key] -> [Rule] -> [Key] -> [Key]
getContainers' acc _ [] = acc
getContainers' acc rules (k:keys) = getContainers' (acc ++ new) rules keys
  where
    containers = getContainersForKey rules k
    new = filter (\el -> not (elem el acc)) containers

getContainersForKey :: [Rule] -> Key -> [Key]
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
