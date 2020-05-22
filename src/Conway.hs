module Conway(playIt) where

-- IMPORTS

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- VARS AND TYPES FOR DISPLAY

width, height, offset, fps :: Int
width = 300
height = 300
offset = 100
fps = 2
boxSize = 30

background :: Color
background = white

window :: Display
window = InWindow "Conway" (width, height) (offset, offset)

-- VARS AND TYPES FOR MODELING

data Cell = Living | Dead deriving Eq

type Coordinate = (Int, Int)
type Coordinates = [Coordinate]
type Row = [Cell]
type Board = [Row]
type CellWithNeighborCount = (Cell, Int)

simulationState = []

-- GENERAL HELPERS

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx val (hd:tail)
  | idx == 0 = val:tail
  | otherwise = hd:replaceNth (idx-1) val tail

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd func list = zipWith func list [0..]

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
    x == 1 || y == 2]

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

boardOfSize :: Int -> Board
boardOfSize size = do
  let blankRow = take size (repeat Dead)
  take size (repeat blankRow)

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

nextValue :: CellWithNeighborCount -> Cell
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

rect :: Picture
rect = rectangleSolid boxSize boxSize

blackRect :: Picture
blackRect = color black rect

redRect :: Picture
redRect = color red rect

rectForCell :: Cell -> Picture
rectForCell Living = redRect
rectForCell Dead = blackRect

place :: Int -> Int -> Picture -> Picture
place x y pic = translate (xf * 10) (yf * 10) pic
  where xf = fromIntegral x :: Float
        yf = fromIntegral y :: Float

spotPic :: Int -> Cell -> Int -> Picture
spotPic y cell x = place x y (rectForCell cell)

rowPic :: Row -> Int -> Picture
rowPic row x = pictures (mapInd iterator row)
    where iterator = spotPic x

draw :: Board -> Picture
draw board = pictures (mapInd rowPic board)

handleInput _ state = state

update :: Float -> [a0] -> [a0]
update seconds game = game

playIt :: IO ()
playIt = do
  putStrLn "How large should the board be?"
  boardSize <- getLine
  let blankBoard = boardOfSize (read boardSize)
  let seedLife = makeSeeds (read boardSize)
  let initialBoard = setBoardLife blankBoard seedLife

  play window background fps initialBoard draw handleInput getNextBoard
