module Main
  ( main
  ) where

filterDigits :: String -> String
filterDigits = filter (\x -> elem x ['0' .. '9'])

takeEnds :: String -> String
takeEnds xs = [head xs, last xs]

toNum :: String -> Int
toNum x = read (takeEnds (filterDigits x))

processLines :: [String] -> [Int]
processLines [] = []
processLines (x:xs) = toNum x : (processLines xs)

solvePart1 :: [String] -> Int
solvePart1 xs = sum $ processLines xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ solvePart1 $ lines contents
