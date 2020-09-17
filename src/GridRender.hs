module GridRender(drawGrid) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Util
import GridUtil

boxSize :: Float
boxSize = 100

square :: Picture
square = rectangleSolid boxSize boxSize

cellToSquare :: Cell -> Picture
cellToSquare 'R' = color red square
cellToSquare 'G' = color green square
cellToSquare 'E' = color white square

place :: Int -> Int -> Picture -> Picture
place x y pic = translate (xf * 10) (yf * 10) pic
  where xf = fromIntegral x :: Float
        yf = fromIntegral y :: Float

cellPic :: Int -> Cell -> Int -> Picture
cellPic x cell y = place x y (cellToSquare cell)

rowPic :: Row -> Int -> Picture
rowPic row x = pictures (mapWithIdx iterator row)
    where iterator = cellPic x

drawGrid :: Grid -> Picture
drawGrid grid = pictures (mapWithIdx rowPic grid)
