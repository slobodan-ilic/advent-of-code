module Lib where

import Data.List.Split
import qualified Data.Map as M

type Seg = [Int]

type Segs = [Seg]

data Rule
  = Leaf Char
  | Node [Seg]
  deriving (Show)

type Rules = M.Map Int Rule

process :: [String] -> [String] -> [String]
process cs pics = filter (\pic -> pic `elem` cs) pics

combs :: Rules -> Seg -> [String]
combs rs [] = [""]
combs rs seg@(id:ids) =
  case rule of
    Leaf c -> [c : r | r <- rest]
    Node segs ->
      [s ++ r | r <- rest, s <- concat [combs rs seg' | seg' <- segs]]
  where
    rule = M.findWithDefault (Leaf 'X') id rs
    rest = combs rs ids

-- parse :: String -> (Rules, [String])
-- parse contents = (rules, pts !! 1)
--   where
--     lns = lines contents
--     pts = splitOn [""] lns
--     rules = M.fromList $ map parseRule (pts !! 0)
parseRule :: String -> (Int, Rule)
parseRule str = (id, rule)
  where
    id = read (takeWhile (/= ':') str) :: Int
    rest = drop 2 $ dropWhile (/= ':') str
    ptsAny = map (dropWhile (== ' ')) (splitOn "|" rest)
    rule =
      case '"' `elem` rest of
        True -> Leaf (rest !! 1)
        False -> Node $ map parseSeg ptsAny

parseSeg :: String -> Seg
parseSeg str = nums
  where
    segs = filter (/= "") $ splitOn " " str
    nums = map (\s -> read s :: Int) segs
