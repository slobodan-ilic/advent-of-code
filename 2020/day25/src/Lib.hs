module Lib where

doorPublicKey :: Int
doorPublicKey = 17807724

transformSubjectNumber :: Int -> Int -> Int
transformSubjectNumber ls sn = tsn 1 ls sn

tsn :: Int -> Int -> Int -> Int
tsn acc 0 _ = acc
tsn acc ls sn = tsn ((acc * sn) `mod` 20201227) (ls - 1) sn

getLoopSize :: Int -> Int
getLoopSize sn = gls 1 7 sn

gls :: Int -> Int -> Int -> Int
gls ls psn sn
  | psn == sn = ls
  | otherwise = gls (ls + 1) sn' sn
  where
    sn' = tsn psn 1 7
