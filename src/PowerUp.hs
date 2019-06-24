module PowerUp where

import Graphics.Gloss
import Ball
import Window
import Control.Concurrent
import Control.Concurrent.STM


data PowerUpEnum = None | BigBar | SmallBar | FastBall | SlowBall deriving Eq

data PowerUp = PUI Position PowerUpEnum

powerUpSpeed :: Float
powerUpSpeed = 30

powerUpSize :: Float
powerUpSize = 5

powerUpColor :: Color
powerUpColor = orange

-- create circle draw
powerUpBall :: Position -> Picture
powerUpBall (x, y) = translate x y $ color powerUpColor $ circleSolid powerUpSize

-- update power up position (falling)
movePowerUp :: Float -> PowerUp -> Position
movePowerUp segundos (PUI (xi, yi) typePower) = (xf, yf)
    where
        xf = xi
        yf = yi - powerUpSpeed * segundos

initializePowerUps :: PowerUp
initializePowerUps = PUI (0,0) BigBar

getPowerUpType :: PowerUp -> PowerUpEnum
getPowerUpType (PUI a b) = b

getNewRandomType :: Int -> PowerUpEnum
getNewRandomType int = (getPUEnumType int)

getPowerUpLocation :: PowerUp -> Position
getPowerUpLocation (PUI (x,y) b) = (x,y) 

getPUEnumType :: Int -> PowerUpEnum
getPUEnumType i
    | i == 1 = BigBar
    | i == 2 = SmallBar
    | i == 3 = FastBall
    | i == 4 = SlowBall    
    


    

    

