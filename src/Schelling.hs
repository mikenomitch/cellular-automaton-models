module Schelling(run) where

-- IMPORTS

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Util
import GridUtil
import GridRender

-- VARS AND TYPES FOR DISPLAY

title = "Schelling's Segregation Model"

width, height, offset, fps :: Int
width = 300
height = 300
offset = 100
fps = 2

background :: Color
background = white

window :: Display
window = InWindow title (width, height) (offset, offset)

-- INITIAL GRID

seedCell :: Int -> Int -> (Int, Int) -> Cell
seedCell redGreenRatio emptyPercentage (emptyIntRandom, colorIntRandom) = if (abs (emptyIntRandom `mod` 100)) + 1 < emptyPercentage
    then 'E'
    else if (abs (colorIntRandom `mod` 100)) + 1 < redGreenRatio then 'R' else 'G'

seedRow :: Int -> Int -> Int -> Int -> Int -> Row
seedRow size x seed redGreenRatio emptyPercentage = map (seedCell redGreenRatio emptyPercentage) randomList where
    randomEmptyList = take size (randoms (mkStdGen (seed + x)))
    randomColorList = take size (randoms (mkStdGen (seed + x + size)))
    randomList = zip randomColorList randomEmptyList

makeGrid :: Int -> Int -> Int -> Int -> Grid
makeGrid size seed redGreenRatio emptyPercentage = do
  [ seedRow size idx seed redGreenRatio emptyPercentage | idx <- [0..size-1] ]

-- ==== LIFE CYCLES ====

-- Finding cells that will move

neighborOfSameType :: Grid -> Cell -> Coordinate -> Bool
neighborOfSameType grid comparisonCell coord = do
  if invalidCoordinate grid coord then
    False
  else
    (getCell grid coord) == comparisonCell

similarNeighborCount :: Grid -> Coordinate -> Int
similarNeighborCount grid (rowIdx, colIdx) = do
  let ownValue = getCell grid (rowIdx, colIdx)
  let neighborCoordinates = getNeighborCoordinates (rowIdx, colIdx)
  length (filter (neighborOfSameType grid ownValue) neighborCoordinates)

isMover :: Int -> Grid -> Coordinate -> Bool
isMover moveThreshold grid coord = if (getCell grid coord) == 'E'
    then False
    else (similarNeighborCount grid coord) < moveThreshold

moverCoordinates :: Grid -> Int -> Coordinates
moverCoordinates grid moveThreshold = matchingCoordinates (isMover moveThreshold) grid

-- Finding empty cells to move to

isEmpty :: Grid -> Coordinate -> Bool
isEmpty grid coord = (getCell grid coord) == 'E'

moveToEmptyCell :: Grid -> Coordinate -> Grid
moveToEmptyCell grid moverCoord = do
  let emptiesList = shuffle (matchingCoordinates isEmpty grid)
  let emptyCoordinate = (head emptiesList)
  swapValuesAtCoordinates grid moverCoord emptyCoordinate

getNextGridState :: Int -> Float -> Grid -> Grid
getNextGridState moveThreshold _ grid = do
  foldl moveToEmptyCell grid (moverCoordinates grid moveThreshold)

-- DRAWING AND RENDERING

draw :: Grid -> Picture
draw grid = drawGrid grid

handleInputNoop _ state = state

run :: IO ()
run = do
  putStrLn "How large should the grid be?"
  gridSize <- getLine

  putStrLn "What percentage of spaces should be red vs green?"
  redGreenRatio <- getLine

  putStrLn "What percentage of spaces should be empty?"
  emptyPercentage <- getLine

  putStrLn "How many similar neighbords does a cell need to stay?"
  moveThreshold <- getLine

  putStrLn "What is your seed for pseudorandomness?"
  seed <- getLine

  let initialGrid = makeGrid (read gridSize) (read seed) (read redGreenRatio) (read emptyPercentage)
  let getNextStateFn = getNextGridState (read moveThreshold)

  play window background fps initialGrid draw handleInputNoop getNextStateFn
