module Lib
  ( marker
  ) where

import Data.List

marker :: String -> Int
marker [] = 0
marker txt = go 0 txt
  where
    go i txt =
      if samelen
        then i + 14
        else go (i + 1) txt
      where
        samelen = (length $ nub $ take 14 $ drop i txt) == 14
