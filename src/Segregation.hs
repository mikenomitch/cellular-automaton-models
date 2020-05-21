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

simulationState = []

draw :: [a0] -> Picture
draw _state = rectangleSolid 20 10

handleInput _ state = state

update :: Float -> [a00] -> [a00]
update seconds game = game

showModel :: IO ()
showModel = play window background fps simulationState draw handleInput update
