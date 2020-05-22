module Conway(run) where

-- IMPORTS

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- DATA AND TYPES FOR DISPLAY

title = "Conway's Game of Life"

width, height, offset, fps :: Int
width = 300
height = 300
offset = 100
fps = 1

boxSize :: Float
boxSize = 30

background :: Color
background = white

window :: Display
window = InWindow title (width, height) (offset, offset)

-- DATA AND TYPES FOR MODELING

data Cell = Living | Dead deriving Eq

type Coordinate = (Int, Int)
type Coordinates = [Coordinate]
type Row = [Cell]
type Board = [Row]

-- GENERAL HELPERS

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx val (hd:tail)
  | idx == 0 = val:tail
  | otherwise = hd:replaceNth (idx-1) val tail

mapWithIdx :: (a -> Int -> b) -> [a] -> [b]
mapWithIdx func list = zipWith func list [0..]

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

addLife :: Int -> Int -> Int -> Coordinates
addLife size lifePercentage seed = do
  [ (x,y) |
    x <- [0..(size -1)],
    y <- [0..(size - 1)],
    r <- (take size (randoms (mkStdGen seed))),
    (r `mod` 100) > lifePercentage]

setValueAtCoordinate :: Board -> Coordinate -> Cell -> Board
setValueAtCoordinate board (rowIdx, colIdx) value = do
  let row = board!!rowIdx
  let newRow = replaceNth colIdx value row
  replaceNth rowIdx newRow board

setRowLife :: Board -> Coordinate -> Board
setRowLife board coordinate = do
  setValueAtCoordinate board coordinate Living

setBoardLife :: Board -> Coordinates -> Board
setBoardLife board coords = do
  foldl setRowLife board coords

numToCell :: Int -> Int -> Cell
numToCell percentageLife randomInt = if (abs (randomInt `mod` 100)) + 1 < percentageLife
    then Living
    else Dead

randomRow :: Int -> Int -> Int -> Int -> Row
randomRow size x seed percentageLife = map (numToCell percentageLife) randomList where
    randomList = take size (randoms (mkStdGen (seed + x)))

makeBoard :: Int -> Int -> Int -> Board
makeBoard size seed percentageLife = do
  [ randomRow size idx seed percentageLife | idx <- [0..size-1] ]

-- LIFE CYCLES

valueAtCoordinate :: Board -> Coordinate -> Cell
valueAtCoordinate board (rowIdx, colIdx) = do
  (board!!rowIdx)!!colIdx

hasLife :: Board -> Coordinate -> Bool
hasLife board coordinate = do
  if invalidCoordinate board coordinate then
    False
  else
    valueAtCoordinate board coordinate == Living

neighborCount :: Board -> Coordinate -> Int
neighborCount board (rowIdx, colIdx) = do
  let neighborCoordinates = [ (rowIdx - 1, colIdx),
                              (rowIdx + 1, colIdx),
                              (rowIdx, colIdx - 1),
                              (rowIdx, colIdx + 1) ]
  length (filter (hasLife board) neighborCoordinates)

nextValue :: (Cell, Int) -> Cell
nextValue (Living, 2) = Living
nextValue (Living, 3) = Living
nextValue (Dead, 3) = Living
nextValue _        = Dead

getNextValue :: Board -> Coordinate -> Cell
getNextValue board coordinate = do
  let neighbors = neighborCount board coordinate
  let cell = valueAtCoordinate board coordinate
  nextValue (cell, neighbors)

getNextRow :: Board -> Int -> Row
getNextRow board rowIdx = do
  [ getNextValue board (rowIdx, colIdx) |
    colIdx <- [0..(colBound board)] ]

getNextBoard :: Float -> Board -> Board
getNextBoard something board = do
  [ getNextRow board rowIdx |
    rowIdx <- [0..(rowBound board)] ]

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

draw :: Board -> Picture
draw board = pictures (mapWithIdx rowPic board)

handleInputNoop _ state = state

run :: IO ()
run = do
  putStrLn "How large should the board be?"
  boardSize <- getLine

  putStrLn "What percentage of spaces should have life?"
  percentageLife <- getLine

  putStrLn "What is your seed for pseudorandomness?"
  seed <- getLine

  let initialBoard = makeBoard (read boardSize) (read seed) (read percentageLife)

  play window background fps initialBoard draw handleInputNoop getNextBoard
