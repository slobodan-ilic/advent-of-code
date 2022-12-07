module Lib
  ( blockMap
  , blockSize
  , getSizes
  , part1
  , part2
  ) where

import qualified Data.List as DL
import qualified Data.Map as DM

type Line = String

type Lines = [Line]

type Size = Int

type Name = String

type File = (Size, Name)

type Files = [File]

type Dirs = [Dir]

data Dir
  = Nil
  | Dir Files Dirs
  deriving (Show)

type Path = String

type Commands = Lines

type Output = Lines

data Block =
  Block Commands Output
  deriving (Show)

type Blocks = [Block]

type BlockMap = DM.Map Path Block

part1 :: Lines -> Int
part1 lns = sum [s | s <- getSizes $ blockMap "" lns, s < 100000]

-- part2 lns = head [s | s <- sizes, 70000000 - total + s >= 30000000]
-- part2 :: Lines -> [Int]
part2 :: Lines -> Int
part2 lns = head [s | s <- sizes, 70000000 - total + s >= 30000000]
  where
    bm = blockMap "" lns
    sizes = DL.sort $ getSizes bm
    total = maximum sizes

getSizes :: BlockMap -> [Int]
getSizes bm = [blockSize bm path | path <- keys]
  where
    keys = DM.keys bm

-- blockSize :: BlockMap -> Path -> [Path]
-- blockSize bm path = dirs
blockSize :: BlockMap -> Path -> Int
blockSize bm path = sum sizes + sum [blockSize bm dir | dir <- dirs]
  where
    block = bm DM.! path
    Block _ contents = block
    dirs = [concat [path, drop 4 ln, "/"] | ln <- contents, take 3 ln == "dir"]
    sizes =
      [ read $ (take 1 $ words ln) !! 0 :: Int
      | ln <- contents
      , take 3 ln /= "dir"
      ]

blockMap :: Path -> Lines -> BlockMap
blockMap path lns = DM.fromList $ blocks path lns

blocks :: Path -> Lines -> [(Path, Block)]
blocks _ [] = []
blocks prevPath xs =
  concat [[(thisPath, Block cmds listing)], blocks thisPath rest]
  where
    cmds = takeWhile (\(x:xs) -> x == '$') xs
    listing = takeWhile (\(x:xs) -> x /= '$') $ drop (length cmds) xs
    rest = drop (length cmds + length listing) xs
    thisPath = updatePath prevPath cmds

updatePath :: Path -> Commands -> Path
updatePath path [] = path
updatePath path (cmd:rest) =
  if take 4 cmd == "$ cd"
    then updatePath current rest
    else updatePath path rest
  where
    cdArg = drop 5 cmd
    current =
      if cdArg == ".."
        then reverse $ dropWhile (/= '/') $ drop 1 $ reverse path
        else concat
               [ path
               , cdArg
               , if cdArg == "/"
                   then ""
                   else "/"
               ]
