module Lib where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coordinate = (Int, Int, Int, Int)

data State
  = Active
  | Inactive
  deriving (Show, Eq)

data PocketCube =
  PocketCube Coordinate State
  deriving (Show)

type PocketDimension = M.Map Coordinate PocketCube

executeCycle :: PocketDimension -> PocketDimension
executeCycle pd = M.fromList [(c, pc) | pc@(PocketCube c s) <- new, s == Active]
  where
    ks = prepareAllKeysForCycle pd
    ncs = map (\k -> M.findWithDefault (PocketCube k Inactive) k pd) ks
    new = map (\pc -> changeCubeState pd pc) ncs

prepareAllKeysForCycle :: PocketDimension -> [Coordinate]
prepareAllKeysForCycle pd = S.elems $ S.fromList aks
  where
    ks = M.keys pd
    anks = concat $ map getNeighborCoords ks
    aks = ks ++ anks

initState :: [PocketCube] -> PocketDimension
initState pcs = M.fromList [(c, pc) | pc@(PocketCube c s) <- pcs, s == Active]

parse :: [String] -> [[PocketCube]]
parse ls = pcls
  where
    pcls = map parseLine [(i, ln) | (ln, i) <- zip ls [0 ..]]

parseLine :: (Int, String) -> [PocketCube]
parseLine (i, ln) = pcs
  where
    pcs = map parseCube [((i, j), c) | (c, j) <- zip ln [0 ..]]

parseCube :: ((Int, Int), Char) -> PocketCube
parseCube ((i, j), c) = PocketCube co s
  where
    s =
      case c of
        '.' -> Inactive
        '#' -> Active
    co = (i, j, 0, 0)

changeCubeState :: PocketDimension -> PocketCube -> PocketCube
changeCubeState pd (PocketCube c s) = PocketCube c s'
  where
    nc = getNeighborCoords c
    ns = M.filterWithKey (\k _ -> k `elem` nc) pd
    n = M.foldr (\(PocketCube _ s) acc -> acc + (fromEnum $ s == Active)) 0 ns
    s' =
      case s of
        Active ->
          if n == 2 || n == 3
            then Active
            else Inactive
        Inactive ->
          if n == 3
            then Active
            else Inactive

getNeighborCoords :: Coordinate -> [Coordinate]
getNeighborCoords (x, y, z, w) = coords
  where
    ds = [1, 0, (-1)]
    coords =
      [ (x + dx, y + dy, z + dz, w + dw)
      | dx <- ds
      , dy <- ds
      , dz <- ds
      , dw <- ds
      , (dx, dy, dz, dw) /= (0, 0, 0, 0)
      ]
