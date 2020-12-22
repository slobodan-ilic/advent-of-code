module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ts = parse contents
      cs = getCorners ts
      grid = arrange (cs !! 0) ts
      lns = coerce $ orient $ arrange (cs !! 0) ts
      -- prepare = (rl' $ rl' $ rl' $ reverse lns)
      prepare = (rl' $ reverse lns)
      ms =
        [ ((i, j), isMonsterAt i j prepare)
        | i <- [0 .. (length lns - 3 - 1)]
        , j <- [0 .. (length (lns !! 0) - 20 - 1)]
        , isMonsterAt i j prepare == True
        ]
      nXes = length $ filter (== '#') $ concat prepare
   -- in putStrLn $ show $ isMonsterAt 16 2 prepare
   in putStrLn $ show $ (nXes, length ms, nXes - (length ms) * 15)
   -- in putStrLn $ show $ isMonsterAt 2 2 lns
