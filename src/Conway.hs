module Conway(run) where

-- IMPORTS

import Util
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- DATA AND TYPES FOR DISPLAY

title = "Conway's Game of Life"

width, height, offset, fps :: Int
width = 300
height = 300
offset = 0
fps = 5

boxSize :: Float
boxSize = 100

background :: Color
background = white

window :: Display
window = InWindow title (width, height) (offset, offset)

-- DATA AND TYPES FOR MODELING

data Cell = Living | Dead deriving Eq

type Coordinate = (Int, Int)
type Coordinates = [Coordinate]
type Row = [Cell]
type Grid = [Row]

-- DOMAIN HELPERS

colBound :: Grid -> Int
colBound grid = (length (grid!!0)) - 1

rowBound :: Grid -> Int
rowBound grid = (length grid) - 1

invalidIdx :: Int -> Int -> Bool
invalidIdx bound val = val > bound || val < 0

invalidRowIdx :: Grid -> Int -> Bool
invalidRowIdx grid val = invalidIdx (rowBound grid) val

invalidColIdx :: Grid -> Int -> Bool
invalidColIdx grid val = invalidIdx (colBound grid) val

invalidCoordinate :: Grid -> Coordinate -> Bool
invalidCoordinate grid (rowIdx, colIdx) = do
  invalidRowIdx grid rowIdx || invalidColIdx grid colIdx

-- SETUP

numToCell :: Int -> Int -> Cell
numToCell percentageLife randomInt = if (abs (randomInt `mod` 100)) + 1 < percentageLife
    then Living
    else Dead

randomRow :: Int -> Int -> Int -> Int -> Row
randomRow size x seed percentageLife = map (numToCell percentageLife) randomList where
    randomList = take size (randoms (mkStdGen (seed + x)))

makeGrid :: Int -> Int -> Int -> Grid
makeGrid size seed percentageLife = do
  [ randomRow size idx seed percentageLife | idx <- [0..size-1] ]

-- LIFE CYCLES

getCell :: Grid -> Coordinate -> Cell
getCell grid (rowIdx, colIdx) =
  grid!!rowIdx!!colIdx

hasLife :: Grid -> Coordinate -> Bool
hasLife grid coordinate = do
  if invalidCoordinate grid coordinate then
    False
  else
    getCell grid coordinate == Living

neighborCount :: Grid -> Coordinate -> Int
neighborCount grid (rowIdx, colIdx) = do
  let neighborCoordinates = [ (rowIdx - 1, colIdx),
                              (rowIdx + 1, colIdx),
                              (rowIdx, colIdx - 1),
                              (rowIdx, colIdx + 1),
                              (rowIdx + 1, colIdx + 1),
                              (rowIdx - 1, colIdx + 1),
                              (rowIdx - 1, colIdx - 1),
                              (rowIdx + 1, colIdx - 1)]
  length (filter (hasLife grid) neighborCoordinates)

nextValue :: (Cell, Int) -> Cell
nextValue (Living, 2) = Living
nextValue (Living, 3) = Living
nextValue (Dead, 3) = Living
nextValue _        = Dead

getNextValue :: Grid -> Coordinate -> Cell
getNextValue grid coordinate = do
  let neighbors = neighborCount grid coordinate
  let cell = getCell grid coordinate
  nextValue (cell, neighbors)

getNextRow :: Grid -> Int -> Row
getNextRow grid rowIdx = do
  [ getNextValue grid (rowIdx, colIdx) |
    colIdx <- [0..(colBound grid)] ]

getNextGrid :: Float -> Grid -> Grid
getNextGrid something grid = do
  [ getNextRow grid rowIdx |
    rowIdx <- [0..(rowBound grid)] ]

-- DRAWING AND RENDERING

square :: Picture
square = rectangleSolid boxSize boxSize

cellToSquare :: Cell -> Picture
cellToSquare Living = color red square
cellToSquare Dead = color black square

place :: Int -> Int -> Picture -> Picture
place x y pic = translate (xf * 10) (yf * 10) pic
  where xf = fromIntegral x :: Float
        yf = fromIntegral y :: Float

cellPic :: Int -> Cell -> Int -> Picture
cellPic y cell x = place x y (cellToSquare cell)

rowPic :: Row -> Int -> Picture
rowPic row x = pictures (mapWithIdx iterator row)
    where iterator = cellPic x

draw :: Grid -> Picture
draw grid = pictures (mapWithIdx rowPic grid)

handleInputNoop _ state = state

run :: IO ()
run = do
  putStrLn "How large should the grid be?"
  gridSize <- getLine

  putStrLn "What percentage of spaces should have life?"
  percentageLife <- getLine

  putStrLn "What is your seed for pseudorandomness?"
  seed <- getLine

  let initialGrid = makeGrid (read gridSize) (read seed) (read percentageLife)

  play window background fps initialGrid draw handleInputNoop getNextGrid
