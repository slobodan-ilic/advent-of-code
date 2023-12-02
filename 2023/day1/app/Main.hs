module Main
  ( main
  ) where

filterDigits :: String -> String
filterDigits = filter (\x -> elem x ['1' .. '9'])

takeEnds :: String -> String
takeEnds xs = [head xs, last xs]

toNum :: String -> Int
toNum x = read (takeEnds (filterDigits x))

processLines :: [String] -> [Int]
processLines [] = []
processLines (x:xs) = toNum x : (processLines xs)

solvePart1 :: [String] -> Int
solvePart1 xs = sum $ processLines xs

mapping :: [(String, Char)]
mapping =
  [ ("one", '1')
  , ("two", '2')
  , ("three", '3')
  , ("four", '4')
  , ("five", '5')
  , ("six", '6')
  , ("seven", '7')
  , ("eight", '8')
  , ("nine", '9')
  ]

singleWordToNumeral :: String -> Maybe Char
singleWordToNumeral line =
  if numerals == []
    then Nothing
    else Just $ head numerals
  where
    numerals = [n | (w, n) <- mapping, take (length w) line == w]

singleWordToNumeralFromEnd :: String -> Maybe Char
singleWordToNumeralFromEnd line =
  if numerals == []
    then Nothing
    else Just $ head numerals
  where
    numerals =
      [n | (w, n) <- mapping, reverse (take (length w) (reverse line)) == w]

firstNumeral :: String -> Char
firstNumeral line =
  if elem x ['1' .. '9']
    then x
    else case y of
           Nothing -> firstNumeral $ tail line
           Just n -> n
  where
    x = head line
    y = singleWordToNumeral line

lastNumeral :: String -> Char
lastNumeral line =
  if elem x ['1' .. '9']
    then x
    else case y of
           Nothing -> lastNumeral $ init line
           Just n -> n
  where
    x = last line
    y = singleWordToNumeralFromEnd line

processLine :: String -> String
processLine [] = []
processLine line = [firstNumeral line, lastNumeral line]

processLines2 :: [String] -> [Int]
processLines2 [] = []
processLines2 (x:xs) = toNum (processLine x) : (processLines2 xs)

solvePart2 :: [String] -> Int
solvePart2 xs = sum $ processLines2 xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- print $ solvePart1 $ lines contents
  print $ solvePart2 $ lines contents
