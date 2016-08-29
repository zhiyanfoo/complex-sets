module Main(main, PongGame(..), render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Draw a pong game state (convert it to a picture).
render :: PongGame -- ^ The game state to render.
       -> Picture -- ^ A picture of this game state.
render game = 
    pictures [ball, walls,
                 mkPaddle rose 120 $ player1 game,
                 mkPaddle orange (-120) $ player2 game]
    where
        -- The pong ball.
        ball = uncurry translate (ballLoc game) $ color ballColor $
            circleSolid 10
        ballColor = dark red

        -- The bottom and top walls.
        wall :: Float -> Picture
        wall offset = 
            translate 0 offset $
                color wallColor $
                    rectangleSolid 270 10

        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        -- Make a padde of a given border and vertical offset.
        mkPaddle :: Color -> Float -> Float -> Picture
        mkPaddle col x y = pictures
          [ translate x y $ color col $ rectangleSolid 26 86
          , translate x y $ color paddleColor $ rectangleSolid 20 80
          ]

        paddleColor = light $ light blue

-- | Initialize the game with this game state.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (50, 0)
  , player1 = 0
  , player2 = 0
  , pause = False
  , up = False
  , down = False
  }

-- | Data describing the state of the pong game.
data PongGame = Game
    { ballLoc :: (Float, Float) -- ^ Pong ball (x, y) location.
    , ballVel :: (Float, Float) -- ^ Pong ball (x, y) velocity.
    , player1 :: Float -- ^ Left player paddle height.
                       -- Zero is the middle of the screen
    , player2 :: Float -- ^ Right player paddle height.
    , pause :: Bool -- ^ if `True` game is paused
    , up :: Bool -- ^ if `True` move player2 (paddle) up
    , down :: Bool -- ^ if `True` move player2 (paddle) down 
    } deriving Show

-- | Update the ball position using its current velocity.
moveBall :: Float -- ^ The number of seconds since last update.
         -> PongGame -- ^ The inital game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game
    | pause game = game
    | up game =  game { player2 = newupplayer2
                      , ballLoc = (x', y') }
    | down game = game {player2 = newdownplayer2 
                       , ballLoc = (x', y') }
    | otherwise = game { ballLoc = (x', y') }
    where
        -- Old locations and velocities.
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        x' = x + vx * seconds
        y' = y +vy * seconds

        newupplayer2 = player2 game + 10
        newdownplayer2 = player2 game - 10

fps :: Int
fps = 60

-- | Detect a collision with a paddle. Upon collisions
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
    where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities
    (vx, vy) = ballVel game

    vx' = if paddleCollision (player1 game) (player2 game) (ballLoc game) radius
          then
              -- Update the velocity.
              -vx
          else
              -- Do nothing. Return the old velocity.
              vx

-- | Detect a collision with one of the side walls. Upon collision,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
    where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
              -- Update the velocity.
              -vy
          else
              -- Do nothing. Return the old velocity.
              vy

-- | Update the game by moving the ball and bouncing off walls.
-- update :: ViewPort -> Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds

type Radius = Float
type Position = (Float, Float)

-- | Given the position and radius of the ball, return whether a collision
-- occured.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollsion
    where 
        topCollision = y - radius <= - fromIntegral width / 2
        bottomCollsion = y + radius >= fromIntegral width / 2

-- | Given the position and radius of the ball and the positions of the two
-- paddles return whether a collision has occured.
paddleCollision :: Float -> Float -> Position -> Radius -> Bool
paddleCollision player1 player2 ballLoc radius = collided
    where 
        player1Collision = collision (120, player1) ballLoc radius
        player2Collision = collision (-120, player2) ballLoc radius
        collision (x1, y1) (x2, y2) r = (x1 - x2)^2 + (y1 - y2) <= r^2
        collided = player1Collision || player2Collision

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- | For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'r') _ _ _) game =
    game { ballLoc = (0, 0) }

-- | For an 'p' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'p') _ _ _) game = 
    game { pause = not $ pause game }

handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game =
    game { up = not $ up game }

handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game =
    game { down = not $ down game }

-- Do nothing for all other events.
handleKeys _ game = game 

main :: IO ()
main = play window background fps initialState render handleKeys update
