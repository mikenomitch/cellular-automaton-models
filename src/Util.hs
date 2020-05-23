module Util(replaceNth, mapWithIdx, shuffle) where

-- GENERAL HELPERS

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx val (hd:tail)
  | idx == 0 = val:tail
  | otherwise = hd:replaceNth (idx-1) val tail

mapWithIdx :: (a -> Int -> b) -> [a] -> [b]
mapWithIdx func list = zipWith func list [0..]

-- TODO: Add a shuffle and apply it where necessary (emptries)
shuffle :: [a] -> [a]
shuffle list = list
