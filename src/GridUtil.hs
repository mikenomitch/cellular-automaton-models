module GridUtil(
  Grid,
  Coordinate,
  Coordinates,
  Row,
  Cell,
  invalidCoordinate,
  colBound,
  rowBound,
  getCell,
  setValueAtCoordinate,
  swapValuesAtCoordinates,
  getNeighborCoordinates,
  matchingCoordinates
) where

import Util

type Cell = Char

type Coordinate = (Int, Int)
type Coordinates = [Coordinate]
type Row = [Cell]
type Grid = [Row]
type CoordinateTester = (Grid -> Coordinate -> Bool)

colBound :: Grid -> Int
colBound board = (length (board!!0)) - 1

rowBound :: Grid -> Int
rowBound board = (length board) - 1

invalidIdx :: Int -> Int -> Bool
invalidIdx bound val = val > bound || val < 0

invalidRowIdx :: Grid -> Int -> Bool
invalidRowIdx board val = invalidIdx (rowBound board) val

invalidColIdx :: Grid -> Int -> Bool
invalidColIdx board val = invalidIdx (colBound board) val

invalidCoordinate :: Grid -> Coordinate -> Bool
invalidCoordinate board (rowIdx, colIdx) = do
  invalidRowIdx board rowIdx || invalidColIdx board colIdx

getCell :: Grid -> Coordinate -> Cell
getCell grid (rowIdx, colIdx) =
  grid!!rowIdx!!colIdx

setValueAtCoordinate :: Grid -> Coordinate -> Cell -> Grid
setValueAtCoordinate grid (rowIdx, colIdx) value = do
  let row = grid!!rowIdx
  let newRow = replaceNth colIdx value row
  replaceNth rowIdx newRow grid

swapValuesAtCoordinates :: Grid -> Coordinate -> Coordinate -> Grid
swapValuesAtCoordinates grid coordA coordB = do
  let valA = getCell grid coordA
  let valB = getCell grid coordB
  let tmpGrid = (setValueAtCoordinate grid coordA valB)
  setValueAtCoordinate tmpGrid coordB valA

getNeighborCoordinates :: Coordinate -> Coordinates
getNeighborCoordinates (rowIdx, colIdx) = do
  [ (rowIdx - 1, colIdx),
    (rowIdx + 1, colIdx),
    (rowIdx, colIdx - 1),
    (rowIdx, colIdx + 1)]

findRowMatchingCoords :: CoordinateTester -> Grid -> Row -> Int -> Coordinates
findRowMatchingCoords testFn grid row rowIdx = filter (testFn grid) coordinatesForRow where
  coordinatesForRow = mapWithIdx (\space colIdx -> (rowIdx, colIdx)) row

matchingCoordinates :: CoordinateTester -> Grid -> Coordinates
matchingCoordinates testFn grid = do
  let findRowMatchingCoords' = (findRowMatchingCoords testFn grid)
  mconcat (mapWithIdx findRowMatchingCoords' grid)