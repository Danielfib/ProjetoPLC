module Ball where

import Graphics.Gloss

-- Construction refers to point of the center of the ball
type Position = (Float, Float)

ballVelocity :: (Float, Float)
ballVelocity = (50, -200)

ballVelocity2 :: (Float, Float)
ballVelocity2 = (60, -360)


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