module Ball where

import Graphics.Gloss

-- Construction refers to point of the center of the ball
type Position = (Float, Float)

ballVelocity :: (Float, Float)
ballVelocity = (45, -200)

ballVelocity2 :: (Float, Float)
ballVelocity2 = (55, 300)

ballVelocitySlow :: (Float, Float)
ballVelocitySlow = (35, 150)

ballVelocityFast :: (Float, Float)
ballVelocityFast = (65, 400)

ballVelocityIncrement :: (Float, Float)
ballVelocityIncrement = (20, 100)

fastenBall :: (Float, Float) -> (Float, Float)
fastenBall (x, y) = (x*1.2, y*1.2)

slowBall :: (Float, Float) -> (Float, Float)
slowBall (x, y) = (x*0.8, y*0.8)

incrementBallVel :: (Float, Float) -> (Float, Float)
incrementBallVel (x, y) = sumOrMinusIncrement (x, y) ballVelocityIncrement

decrementBallVel :: (Float, Float) -> (Float, Float)
decrementBallVel (x, y) = sumOrMinusDecrement (x, y) ballVelocityIncrement

sumOrMinusIncrement :: (Float, Float) -> (Float, Float) -> (Float, Float)
sumOrMinusIncrement (x, y) (i, j)  
    | (x < 0 && y < 0)      = (x-i, y-j)
    | (x < 0 && y >=0)      = (x-i, y+j)
    | (x >= 0 && y < 0)     = (x+i, y-j)
    | (x >= 0 && y >= 0)    = (x+i, y+j)
    
sumOrMinusDecrement :: (Float, Float) -> (Float, Float) -> (Float, Float)
sumOrMinusDecrement (x, y) (i, j)
    | (x < 0 && y < 0)      = (x+i, y+j)
    | (x < 0 && y >=0)      = (x+i, y-j)
    | (x >= 0 && y < 0)     = (x-i, y+j)
    | (x >= 0 && y >= 0)    = (x-i, y-j)


ballSize :: Float
ballSize = 10

ballColor :: Color
ballColor = white

-- create circle draw
ball :: Position -> Picture
ball (x, y) = translate x y $ color ballColor $ circleSolid ballSize

-- Updates ball position
moveBall :: Float -> Position -> Position -> Position
moveBall segundos (x, y) (vx, vy) = (x', y')
    where
        x' = x + vx * segundos
        y' = y + vy * segundos