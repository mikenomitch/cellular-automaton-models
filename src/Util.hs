module Util(replaceNth, mapWithIdx) where

-- GENERAL HELPERS

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx val (hd:tail)
  | idx == 0 = val:tail
  | otherwise = hd:replaceNth (idx-1) val tail

mapWithIdx :: (a -> Int -> b) -> [a] -> [b]
mapWithIdx func list = zipWith func list [0..]
