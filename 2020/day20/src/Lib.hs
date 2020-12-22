module Lib where

import Data.List
import Data.List.Split

data Tile =
  Tile Int [String]
  deriving (Eq)

instance Show Tile where
  show (Tile id lns) =
    "\n\nTile: " ++ show id ++ "\n" ++ (intercalate "\n" (map show lns'))
    where
      lns' = map (\ln -> init $ tail ln) $ init $ tail lns

arrange :: Tile -> [Tile] -> [[Tile]]
arrange corner tiles = arrange' [[corner]] tiles

arrange' :: [[Tile]] -> [Tile] -> [[Tile]]
arrange' acc tiles
  | length (concat acc) == length tiles = acc
  | otherwise = arrange' newAcc tiles
  where
    n = floor . sqrt . fromIntegral $ length tiles
    i = length acc - 1
    j = length (last acc) - 1
    fixtures =
      if i == 0 && j < n - 1
        then [(acc !! i) !! j]
        else if j < n - 1
               then [(acc !! i) !! j, (acc !! (i - 1)) !! (j + 1)]
               else [(acc !! i) !! 0]
    goalTiles =
      filter
        (\t -> not $ elem t (concat acc))
        (if i == 0 || j == n - 1
           then getEdges tiles
           else tiles)
    matches = map (\t -> nMatchesForTile t fixtures) goalTiles
    (_, t) =
      head $
      filter (\(m, t) -> m == length fixtures && (not $ t `elem` (concat acc))) $
      zip matches goalTiles
    newAcc =
      if j < n - 1
        then (init acc) ++ [((last acc) ++ [t])]
        else acc ++ [[t]]

orient :: [[Tile]] -> [[Tile]]
orient b = orient' 0 0 b

orient' :: Int -> Int -> [[Tile]] -> [[Tile]]
orient' i j b
  | (i, j) == (0, 0) = orient' i (j + 1) $ orientInit b
  | j < n - 1 && i <= n - 1 = orient' i (j + 1) nb
  | j == n - 1 && i < n - 1 = orient' (i + 1) 0 nb
  | (i, j) == (n - 1, n - 1) = nb
  | otherwise = []
  where
    n = length b
    nb = newBoard i j b

coerce :: [[Tile]] -> [String]
coerce lns = concat $ map coerceLine lns

monster :: String
monster =
  concat
    ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]

isMonsterAt :: Int -> Int -> [String] -> Bool
isMonsterAt i j lns = all (== '#') flt
  where
    lns' = concat $ map (\ln -> take 20 $ drop j ln) (take 3 $ drop i lns)
    flt = map (\(c, _) -> c) $ filter (\(_, cm) -> cm == '#') $ zip lns' monster

patchAt :: Int -> Int -> [String] -> [String]
patchAt i j lns = map (\ln -> take 20 $ drop j ln) (take 3 $ drop i lns)

coerceLine :: [Tile] -> [String]
coerceLine ts =
  foldr
    (\(Tile _ lns) acc ->
       [ ln ++ al
       | (al, ln) <- zip acc (map (\ln -> init $ tail ln) $ init $ tail lns)
       ])
    a
    ts
  where
    (Tile _ lns) = ts !! 0
    n = length lns - 2
    a = take n $ repeat ""

newBoard :: Int -> Int -> [[Tile]] -> [[Tile]]
newBoard i j board = newBoard
  where
    tl =
      if j /= 0
        then (board !! i) !! (j - 1)
        else (board !! (i - 1)) !! 0
    t = (board !! i) !! j
    nt =
      if j /= 0
        then orientToLeft tl t
        else rl $ rl $ rl (orientToLeft (rl tl) t)
    prev = take i board
    rest = drop (i + 1) board
    line = board !! i
    linePrev = take j line
    lineRest = drop (j + 1) line
    newLine = linePrev ++ [nt] ++ lineRest
    newBoard = prev ++ [newLine] ++ rest

nXes :: Tile -> Int
nXes (Tile _ lns) =
  foldr (\el acc -> acc + fromEnum (el == '#')) 0 (concat lns')
  where
    lns' = map (\ln -> init $ drop 1 ln) (init $ drop 1 lns)

orientToLeft :: Tile -> Tile -> Tile
orientToLeft lt t = nt
  where
    (_, _, _, bl) = getBorders lt
    (b1, b2, b3, b4) = getBorders t
    nt =
      if bl == b3
        then t
        else if bl == reverse b3
               then fud t
               else if bl == b1
                      then fud $ rl t
                      else if bl == reverse b1
                             then rl t
                             else if bl == b4
                                    then fud $ rl $ rl t
                                    else if bl == reverse b4
                                           then rl $ rl t
                                           else if bl == b2
                                                  then rl $ rl $ rl t
                                                  else fud $ rl $ rl $ rl t

orientInit :: [[Tile]] -> [[Tile]]
orientInit ((t:ts):lns) = board
  where
    (b1, b2, b3, b4) = getBorders t
    tr = (board !! 0) !! 1
    td = (board !! 1) !! 0
    t' =
      if hasBorderMatch b4 tr
        then if hasBorderMatch b2 td
               then t
               else fud t
        else if hasBorderMatch b1 tr
               then if hasBorderMatch b4 td
                      then rl $ rl $ rl t
                      else fud $ rl $ rl $ rl t
               else if hasBorderMatch b3 tr
                      then if hasBorderMatch b1 td
                             then rl $ rl t
                             else fud $ rl $ rl t
                      else if hasBorderMatch b2 tr
                             then if hasBorderMatch b3 td
                                    then rl t
                                    else fud $ rl t
                             else t
    board = (t' : ts) : lns

rl :: Tile -> Tile
-- rl (Tile id lns) = Tile id $ (transpose . map reverse) lns
rl (Tile id lns) = Tile id $ rl' lns

rl' :: [String] -> [String]
rl' = transpose . map reverse

fud :: Tile -> Tile
fud (Tile id lns) = Tile id $ reverse lns

getCorners :: [Tile] -> [Tile]
getCorners tiles = ts
  where
    nMatches = map (\t -> nMatchesForTile t tiles) tiles
    ts = [t | (t, nm) <- zip tiles nMatches, nm <= 2]

getEdges :: [Tile] -> [Tile]
getEdges tiles = ts
  where
    nMatches = map (\t -> nMatchesForTile t tiles) tiles
    ts = [t | (t, nm) <- zip tiles nMatches, nm <= 3]

nMatchesForTile :: Tile -> [Tile] -> Int
nMatchesForTile _ [] = 0
nMatchesForTile t (t':ts') =
  if t == t'
    then 0 + nMatchesForTile t ts'
    else m1 + m2 + m3 + m4 + nMatchesForTile t ts'
  where
    (b1, b2, b3, b4) = getBorders t
    m1 = fromEnum $ hasBorderMatch b1 t'
    m2 = fromEnum $ hasBorderMatch b2 t'
    m3 = fromEnum $ hasBorderMatch b3 t'
    m4 = fromEnum $ hasBorderMatch b4 t'

hasBorderMatch :: String -> Tile -> Bool
hasBorderMatch b t =
  (b == b1 || b == reverse b1) ||
  (b == b2 || b == reverse b2) ||
  (b == b3 || b == reverse b3) || (b == b4 || b == reverse b4)
  where
    (b1, b2, b3, b4) = getBorders t

getBorders :: Tile -> (String, String, String, String)
getBorders (Tile _ lns) = (b1, b2, b3, b4)
  where
    b1 = head lns
    b2 = last lns
    b3 = map head lns
    b4 = map last lns

parse :: String -> [Tile]
parse contents = tiles
  where
    lns = lines contents
    tilesParts = splitOn [""] lns
    tiles = map parseTile tilesParts

parseTile :: [String] -> Tile
parseTile lns = Tile id dataLines
  where
    idLine = head lns
    id = read (init (drop 5 idLine)) :: Int
    dataLines = tail lns
