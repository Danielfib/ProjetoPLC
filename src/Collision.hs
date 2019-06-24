module Collision where

import Ball
import Blocks
import Window
import Player
import PowerUp

funIf :: Bool -> a -> a -> a
funIf b x y = if b then x else y

-- Multiplis Tuples
mulTuple :: Num a => (a, a) -> (a, a) -> (a, a)
mulTuple (x1, x2) (y1, y2) = (x1*y1, x2*y2)

-- checks for overlap between two value ranges
overlap :: Ord a => (a, a) -> (a, a) -> Bool
overlap (xmin, xmax) (ymin, ymax) = xmin <= ymax && ymin <= xmax

-- Creates a range of values centered on x
range :: Num a => a -> a -> (a, a)
range x r = (x - r, x + r)

topCollision :: Float -> Position -> Position -> Bool
topCollision seconds (_, y) (_, vy) = (y + vy*seconds) + 2*ballSize > halfHeight

leftCollision :: Float -> Position -> Position -> Bool
leftCollision seconds (x, _) (vx, _) = (x + vx*seconds) - 2*ballSize <= -halfWidth

rightCollision :: Float -> Position -> Position -> Bool
rightCollision seconds (x, _) (vx, _) = (x + vx*seconds) + 2*ballSize >= halfWidth


-- did paddle / ball collided
paddleCollision :: Float -> Float -> Position -> Position -> Bool
paddleCollision seconds playerX (x, y) (vx, vy) = yCollision && xCollision
    where
        yCollision = (y + vy*seconds)  <= playerY + halfPlayerHeight 
                  && (y + vy*seconds)  >= playerY - halfPlayerHeight
        xCollision = (x + vx*seconds)  >= playerX - halfPlayerWidth 
                  && (x + vx*seconds)  <= playerX + halfPlayerWidth

--check power up collision with paddle
powerUpCollision :: Float -> Float -> Position -> Bool
powerUpCollision seconds playerX (x, y) = yCollision && xCollision
    where 
        yCollision = y <= playerY + halfPlayerHeight
                  && y >= playerY - halfPlayerHeight
        xCollision = x <= playerX + halfPlayerWidth
                  && x >= playerX - halfPlayerWidth

pickedUpPowerUP :: Float -> Float -> PowerUp -> Bool
pickedUpPowerUP seconds playerX (PUI (x, y) a) 
    | powerUpCollision seconds playerX (x,y) = True
    | otherwise                              = False
    -- if powerUpCollision seconds playerX (x,y) 
    -- then do -- apply power up
    --     --print("opa")
    --     return True
    --     --if a == BigBar then do playerWidth = 100
    --     --else playerWidth = 40    
    -- else return False

-- if did hit a block
inCorner :: (Num a, Ord a) => a -> (a, a) -> (a, a) -> Bool
inCorner x (xmin, xmax) (rmin, rmax) = (xmin > x + rmin && xmin < x + rmax)
                                     || (xmax < x - rmin && xmax > x - rmax)

-- hit wall and change velocity
wallBounce :: Float -> Position -> Position -> Position
wallBounce seconds pos bv@(vx, vy)
        | leftCollision seconds pos bv 
            || rightCollision seconds pos bv  = (-vx,  vy)
        | topCollision seconds pos bv         = ( vx, -vy)
        | otherwise                           = ( vx,  vy)

-- paddle / ball collision and changes ball velocity
paddleBounce :: Float -> Position -> Position -> Float -> Float -> Position
paddleBounce seconds bp@(x,y) bv@(vx, vy) pp pv = 
    if paddleCollision seconds pp bp bv
    then (newVx, -vy)
    else bv
    where
        newVx = -250 + 500* (x - (pp - halfPlayerWidth))/playerWidth

-- Changes ball velocity when hitting a block
blockCollision :: Float -> Position -> Position -> Blocks -> (Position)
blockCollision seconds v@(vx,vy) bp@(xball, yball) [] = (v)
blockCollision seconds v@(vx,vy) bp@(xball, yball) (b:bs)
    | hitCornerH xb && overlapY yb = ((-vx,  vy))
    | hitCornerV yb && overlapX xb = (( vx, -vy))
    | hitSide    xb && overlapY yb = ((-vx,  vy))
    | hitTop     yb && overlapX xb = (( vx, -vy))
    | otherwise                    = blockCollision seconds v bp bs
    where
        (xb, yb) = blockPos b
        hitCornerH xb = inCorner xb xballRange (0.8*bHalfWidth, bHalfWidth)
        hitCornerV yb = inCorner yb yballRange (-bHalfHeight, -0.8*bHalfHeight)
        hitSide    xb = inCorner xb xballRange (0.5*bHalfWidth, bHalfWidth)
        hitTop     yb = inCorner yb yballRange (-bHalfHeight, -0.5*bHalfHeight)
        overlapY   yb = overlap yballRange $ range yb bHalfHeight
        overlapX   xb = overlap xballRange $ range xb bHalfWidth
        xballRange    = range (xball) ballSize
        yballRange    = range (yball) (-ballSize)

removeBlocks :: Float ->  Blocks -> Position -> Position -> Blocks
removeBlocks seconds bs (xball, yball) (vx, vy) = filter (not. hit) bs
    where 
        hit (Block (xb, yb)) = overlapBallX (range xb bHalfWidth)
                              && overlapBallY (range yb bHalfHeight)
        xballRange             = range (xball) ballSize
        yballRange             = range (yball) ballSize
        overlapBallX           = overlap xballRange
        overlapBallY           = overlap yballRange