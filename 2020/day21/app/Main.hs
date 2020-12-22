module Main where

import Data.List
import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let entries = parse contents
      allergens = getAllergens entries
      foods = getFoods entries
      -- cca =
      --   map fst $
      --   filter (\(f, cca) -> cca == False) $
      --   zip foods $ checkFoods foods allergens entries
      -- appearances = concat $ map fst entries
      -- nOcc = sum $ map (\f -> length $ filter (\a -> f == a) appearances) cca
      -- list =
      --   intercalate "," $
      --   map fst $ sortOn snd $ checkFoods foods allergens entries
      sorted =
        sortOn (\(_, pas) -> length pas) $ checkFoods foods allergens entries
      cleaned = clean sorted
   in putStrLn $ show $ intercalate "," $ map fst $ sortOn snd cleaned
  -- mapM_ print $ parse contents
