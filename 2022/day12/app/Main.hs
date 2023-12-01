module Main
  ( main
  ) where

import Data.Char

type Pos = (Int, Int)

paths :: [String] -> [Pos] -> [[Pos]]
paths topo sofar =
  case current of
    'E' -> [sofar]
    _ -> concat [paths topo (sofar ++ [adj]) | adj <- adjacent]
    -- _ -> paths topo (sofar ++ [adjacent !! 0])
  where
    (i, j) = last sofar
    current = (topo !! i) !! j
    adjacent = getFeasible topo sofar

getFeasible :: [String] -> [Pos] -> [Pos]
getFeasible topo sofar = adjacent
  where
    (i, j) = last sofar
    curr = (topo !! i) !! j
    co = ord curr
    i' = max 0 (i - 1)
    j' = max 0 (j - 1)
    i'' = min (length topo - 1) (i + 1)
    j'' = min (length (topo !! 0) - 1) (j + 1)
    adjacent =
      filter
        (\(ii, jj) ->
           (curr == 'S') ||
           ((if (ii, jj) == (20, 148)
           -- ((if (ii, jj) == (2, 5)
               then ord 'z'
               else ord ((topo !! ii) !! jj)) -
            co <=
            1)) $
      filter (\x -> not $ elem x sofar) $
      filter (/= (i, j)) [(i, j'), (i, j''), (i', j), (i'', j)]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let topo = lines contents
      pts = paths topo [(20, 0)]
      -- pts = paths topo [(0, 0)]
      nmin = minimum $ map length pts
      -- finPths = filter (\(i, j) -> (topo !! i) !! j == 'E') pts
   in putStrLn $ show $ nmin
   -- in putStrLn $ show $ head $ dropWhile (\x -> length x /= nmin) pts
   -- in mapM_ (putStrLn . show) $ paths topo [(0, 0)]
