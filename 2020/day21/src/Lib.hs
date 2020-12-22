module Lib where

import Data.List
import Data.List.Split

type Foods = [String]

type Allergens = [String]

type Entry = (Foods, Allergens)

parse :: String -> [Entry]
parse contents = map parseLine $ lines contents

parseLine :: String -> Entry
parseLine ln = (foods, allergens)
  where
    pts = splitOn " (" ln
    foods = words $ pts !! 0
    allergens = splitOn ", " $ init $ drop 9 $ pts !! 1

getAllergens :: [Entry] -> [String]
getAllergens entries = nub $ concat $ map snd entries

getFoods :: [Entry] -> [String]
getFoods entries = nub $ concat $ map fst entries

checkFoods :: [String] -> [String] -> [Entry] -> [(String, [String])]
checkFoods foods allergens entries = pas
  where
    pas =
      filter (\(_, as) -> length as /= 0) $
      map (\food -> checkFood food allergens entries) foods
    -- res = [(f, as) | (f, as) <- zip foods pas, length asj]

-- sortAndClean :: [(String, [String])] -> [(String, [String])]
-- sortAndClean checked = clean sorted
--   where
--     sorted = sortOn (\(_, pas) -> length pas) checked
-- clean :: [(String, [String])] -> [(String, [String])]
clean sorted =
  if length fixed == length sorted
    then sorted
    else clean cleaned
  where
    fixed =
      map (\(_, algs) -> algs !! 0) $
      filter (\(_, algs) -> length algs == 1) sorted
    cleaned =
      map
        (\(f, algs) ->
           ( f
           , if length algs == 1
               then algs
               else filter (\a -> not $ a `elem` fixed) algs))
        sorted

checkFood :: String -> [String] -> [Entry] -> (String, [String])
checkFood food allergens entries = (food, res)
  where
    ccas = map (\allergen -> canContainAllergen food allergen entries) allergens
    res = map snd $ filter (\(cca, a) -> cca == True) $ zip ccas allergens

canContainAllergen :: String -> String -> [Entry] -> Bool
canContainAllergen _ _ [] = True
canContainAllergen food allergen (entry:entries)
  | allergen `elem` allergens =
    if food `elem` foods
      then canContainAllergen food allergen entries
      else False
  | otherwise = canContainAllergen food allergen entries
  where
    (foods, allergens) = entry
