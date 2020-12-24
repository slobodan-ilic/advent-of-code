module Lib where

import qualified Data.Set as S

data Move
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Show)

type Coord = (Int, Int)

type TileSet = S.Set Coord

getAdj :: Coord -> TileSet
getAdj (x, y) =
  S.fromList
    [ (x + 2, y)
    , (x + 1, y - 1)
    , (x - 1, y - 1)
    , (x - 2, y)
    , (x - 1, y + 1)
    , (x + 1, y + 1)
    ]

play :: Int -> TileSet -> TileSet
play 0 ts = ts
play n ts = play (n - 1) (move ts)

getCandidates :: TileSet -> TileSet
getCandidates ts = adjacent `S.union` ts
  where
    adjacent = S.foldr (\el acc -> acc `S.union` (getAdj el)) S.empty ts

move :: TileSet -> TileSet
move ts = foldr (\c acc -> flipTile acc c) ts cs
  where
    tl = S.toList $ getCandidates ts
    flips = map (\c -> isFlip c ts) tl
    -- flips = map (\c -> isFlip c ts) tl
    (cs, _) = unzip $ filter (\(t, f) -> f == True) $ zip tl flips

isFlip :: Coord -> TileSet -> Bool
isFlip c ts =
  if c `S.member` ts
    then length cs == 0 || length cs > 2
    else length cs == 2
  where
    adj = getAdj c
    cs = adj `S.intersection` ts

process :: [[Move]] -> TileSet
process = go S.empty
  where
    go :: TileSet -> [[Move]] -> TileSet
    go ts [] = ts
    go ts (mvs:rest) = go ts' rest
      where
        c = movesToCoord mvs
        ts' = flipTile ts c

flipTile :: TileSet -> Coord -> TileSet
flipTile ts c =
  if c `S.member` ts
    then S.delete c ts
    else S.insert c ts

movesToCoord :: [Move] -> Coord
movesToCoord = go (0, 0)
  where
    go :: Coord -> [Move] -> Coord
    go acc [] = acc
    go (x, y) (m:ms) = go (x + dx, y + dy) ms
      where
        (dx, dy) =
          case m of
            E -> (2, 0)
            SE -> (1, (-1))
            SW -> ((-1), (-1))
            W -> ((-2), 0)
            NW -> ((-1), 1)
            NE -> (1, 1)

parse :: String -> [[Move]]
parse cont = map parseLine $ lines cont

parseLine :: String -> [Move]
parseLine [] = []
parseLine ('e':cs) = E : (parseLine cs)
parseLine ('w':cs) = W : (parseLine cs)
parseLine (c:c':cs) = d : (parseLine cs)
  where
    d =
      case c of
        's' ->
          case c' of
            'e' -> SE
            'w' -> SW
        'n' ->
          case c' of
            'e' -> NE
            'w' -> NW
