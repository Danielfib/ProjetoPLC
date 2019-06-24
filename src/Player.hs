module Player where

import Graphics.Gloss
import Window

playerAcceleration :: Float
playerAcceleration = 350

playerColor :: Color
playerColor = red

playerWidth :: Float
playerWidth = 60

buffedPlayerWidth :: Float
buffedPlayerWidth = 160

halfPlayerWidth :: Float
halfPlayerWidth = playerWidth / 2

playerHeight :: Float
playerHeight = 10

halfPlayerHeight :: Float
halfPlayerHeight = playerHeight / 2

playerY :: Float
playerY = -halfHeight + 20

-- Player rectangle
mkPlayer :: Float -> Picture
mkPlayer x = translate x playerY $ color playerColor $ rectangleSolid playerWidth playerHeight

-- Updates player location
movePlayer :: Float -> Float -> Float -> Float
movePlayer seconds x v
    | paddleWallCollision (x + deltaX) = x
    | otherwise = x + deltaX
    where
        deltaX = v * seconds

leftWallCollision :: Float -> Bool
leftWallCollision x 
    | x - halfPlayerWidth <= -halfWidth + 5 = True
    | otherwise                             = False

rightWallCollision :: Float -> Bool
rightWallCollision x
    | x + halfPlayerWidth >= halfWidth - 5 = True
    | otherwise                            = False

paddleWallCollision :: Float -> Bool
paddleWallCollision x = leftWallCollision x || rightWallCollision x