-- GENERAL OUTLINE
-- N by N grid
-- Randomly fill it with
  -- R red squares
  -- B blue squares
-- Go square by square
-- get the count of similar neighboring squares
-- Move the square if they are a mover
-- Continue until an equilibrium is reached

-- PLAN OF ATTACK
-- First display an empty box


module Segregation(showModel) where

-- IMPORTS

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- VARS AND TYPES FOR DISPLAY

width, height, offset, fps :: Int
width = 300
height = 300
offset = 100
fps = 5

background :: Color
background = white

window :: Display
window = InWindow "Segregation" (width, height) (offset, offset)

-- VARS AND TYPES FOR MODELING

type Coordinate = (Int, Int)
type Coordinates = [Coordinate]
type Cell = Char
type Row = [Cell]
type Board = [Row]
type CellWithNeighborCount = (Cell, Int)

redCell = 'R'
blueCell = 'B'
emptyCell = '_'

simulationState = []

-- GENERAL HELPERS

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx val (hd:tail)
  | idx == 0 = val:tail
  | otherwise = hd:replaceNth (idx-1) val tail

-- PRINTING HELPERS

printDivider :: IO()
printDivider = putStrLn "==========="

printEnd :: IO()
printEnd = putStrLn "STABLE LIFE"

printRows :: Board -> IO()
printRows (row:[]) = do
  putStrLn row
  printDivider
printRows (row:rest) = do
  putStrLn row
  printRows rest

printBoard :: Board -> IO()
printBoard board = do
  printRows board

-- DOMAIN HELPERS

colBound :: Board -> Int
colBound board = (length (board!!0)) - 1

rowBound :: Board -> Int
rowBound board = (length board) - 1

invalidIdx :: Int -> Int -> Bool
invalidIdx bound val = val > bound || val < 0

invalidRowIdx :: Board -> Int -> Bool
invalidRowIdx board val = invalidIdx (rowBound board) val

invalidColIdx :: Board -> Int -> Bool
invalidColIdx board val = invalidIdx (colBound board) val

invalidCoordinate :: Board -> Coordinate -> Bool
invalidCoordinate board (rowIdx, colIdx) = do
  invalidRowIdx board rowIdx || invalidColIdx board colIdx

-- SETUP

-- CHANGE HERE TO SEE DIFFERENT EVOLUTION
makeSeeds :: Int -> Coordinates
makeSeeds size = do
  [ (x,y) |
    x <- [0..(size -1)],
    y <- [0..(size - 1)],
    (mod (x + y) 3) == 0 || x == y || x == (size - y) ]

setValueAtCoordinate :: Board -> Coordinate -> Cell -> Board
setValueAtCoordinate board (rowIdx, colIdx) value = do
  let row = board!!rowIdx
  let newRow = replaceNth colIdx value row
  replaceNth rowIdx newRow board

boardOfSize :: Int -> Board
boardOfSize size = do
  let blankRow = take size (repeat emptyCell)
  take size (repeat blankRow)

valueAtCoordinate :: Board -> Coordinate -> Cell
valueAtCoordinate board (rowIdx, colIdx) = do
  (board!!rowIdx)!!colIdx

--

draw :: [a0] -> Picture
draw _state = rectangleSolid 20 10

handleInput _ state = state

update :: Float -> [a0] -> [a0]
update seconds game = game

showModel :: IO ()
showModel = play window background fps simulationState draw handleInput update
