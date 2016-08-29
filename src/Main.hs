module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

base :: Int
base = 100

width, height, offset :: Int
width = 4 * base
height = 4 * base
offset = base -- ^ used to define position for the top-left corner of the window.

window :: Display
window = InWindow "Complex Sets" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 60

-- | Data describing the state of the ComplexGraph
data ComplexGraph = Graph
    { points :: [Point] -- ^ points to mark as black
    , area :: (Point, Point)
    } deriving Show

-- | Initialize the game with this game state.
initialState :: ComplexGraph
initialState = Graph
  { points = [(10, 10), (-10, 10)]
  , area = ((-2, -2), (2,2))
  }

-- | Draw the graph (convert it to a picture).
render :: ComplexGraph -- ^ The game state to render.
       -> Picture -- ^ A picture of this game state.
render graph = 
    -- I believe that later pictures go to front
    pictures [axes, p]
    where
        -- graph axes
        verticalAxis = rectangleSolid 1 (fromIntegral height)
        horizontalAxis = rectangleSolid (fromIntegral width) 1
        tick = translate (-100) 4 $ rectangleSolid 1 8
        axes = pictures  [verticalAxis, horizontalAxis, tick]
        p = pictures . map toSquare $ (points graph)

toSquare :: Point -> Picture
toSquare (x, y) = translate x y $ rectangleSolid 1 1

stillnessInTheMove :: Float -> ComplexGraph -> ComplexGraph 
stillnessInTheMove seconds graph =  graph

update :: Float -> ComplexGraph -> ComplexGraph
update seconds =  stillnessInTheMove seconds

-- | Respond to key events.
handleKeys :: Event -> ComplexGraph -> ComplexGraph

-- Do nothing for all other events.
handleKeys _ game = game 

main :: IO ()
main = play window background fps initialState render handleKeys update
