module Lib where

import Data.List.Split

type Game = ([Int], [Int])

parse :: String -> Game
parse contents = (lns1, lns2)
  where
    ps = splitOn [""] $ lines contents
    lns1 = map (\el -> read el :: Int) $ tail (ps !! 0)
    lns2 = map (\el -> read el :: Int) $ tail (ps !! 1)

turn :: Game -> Game
turn ((c1:rest1), (c2:rest2)) =
  if c1 <= length rest1 && c2 <= length rest2
    -- new rec game
    then if length p1' > 0
           then (rest1 ++ [c1, c2], rest2)
           else (rest1, rest2 ++ [c2, c1])
    -- normal turn
    else if c1 > c2
           then (rest1 ++ [c1, c2], rest2)
           else (rest1, rest2 ++ [c2, c1])
  where
    (p1', p2') = play (take c1 rest1, take c2 rest2)

play :: Game -> Game
play game = play' [] game

play' :: [Game] -> Game -> Game
play' prev game@(p1, p2)
  | p1 `elem` (map fst prev) || p2 `elem` (map snd prev) = ([123], [])
  | length p1 > 0 && length p2 > 0 = play' (game : prev) (turn game)
  -- | length p1 > 0 && length p2 > 0 = play' [] (turn game)
  | otherwise = game

calc :: Game -> Int
calc (p1, p2) = sum [c * i | (c, i) <- zip (reverse deck) [1 ..]]
  where
    deck =
      if length p1 > 0
        then p1
        else p2
