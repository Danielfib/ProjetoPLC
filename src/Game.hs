module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Concurrent.STM
import Window
import Ball
import Blocks
import Player
import Collision

data GameStatus = Game
    { ballLoc   :: Position -- (x, y) ball coord
    , ballVel   :: Position -- (x, y) ball velocity
    , playerLoc :: Float    -- player X location
    , playerVel :: Float    -- player velocity
    , playerAcc :: Float    -- player acceleration
    , isPaused  :: Bool     
    , blocks    :: TVar Blocks   -- blocks on screen
    , gameStat  :: Int      -- game status: 0 - in game, 1 = victory, -1 = loss.    
    }

-- initial game state
initialState :: TVar Blocks -> GameStatus
initialState b1 = Game
    { ballLoc   = (0 , -100)
    , ballVel   = ballVelocity
    , playerLoc = 0
    , playerVel = 0
    , playerAcc = playerAcceleration
    , isPaused  = True
    , blocks    = b1
    , gameStat  = 0
    }

render :: GameStatus -> IO (Picture)
render game = do
    bl1 <- atomically $ readTVar $ blocks game
    return (pictures [ballPics, walls, playerPics, pictures[drawBlocks bl1], msgPic])
    where
        ballPics   = pictures [ ball $ ballLoc game ]
        playerPics = pictures [ mkPlayer $ playerLoc game ]
        msgPic     = curMsg (gameStat game) (isPaused game) 

updateBall :: Float -> GameStatus -> IO (GameStatus)
updateBall seconds game = return $ game { ballLoc = moveBall seconds pos v }
    where pos = ballLoc game
          v   = ballVel game

updatePlayer :: Float -> GameStatus -> IO (GameStatus)
updatePlayer seconds game = return $ game { playerLoc = movePlayer seconds x v }
    where x = playerLoc game
          v = playerVel game

updateWall :: Float -> GameStatus -> IO (GameStatus)
updateWall seconds game = return $ game { ballVel = wallBounce seconds pos v }
    where pos = ballLoc game
          v   = ballVel game

updatePaddle :: Float -> GameStatus -> IO (GameStatus)
updatePaddle seconds game = return $ game { ballVel = paddleBounce seconds bp bv pp pv }
    where bp = ballLoc   game
          bv = ballVel   game
          pp = playerLoc game
          pv = playerVel game

updateBlocks :: Float -> GameStatus -> IO (GameStatus)
updateBlocks seconds game = do
    ((v1x, v1y), pow1) <- atomically $ do
        bl1 <- readTVar $ blocks game
        let (ballVel', p1) = blockCollision seconds bv bp bl1
        writeTVar (blocks game) (removeBlocks seconds bl1 bp bv)
        return (ballVel', p1)
    
    let v1 = (v1x, v1y)

    return $ game { ballVel = v1}
        where -- update ball velocity when hit block
            bv = ballVel game
            bp = ballLoc game


createPowerUp :: GameStatus -> IO (GameStatus)
createPowerUp game = do
    atomically $ do
        bl1 <- readTVar $ blocks game
        if length bl1 == 0 then return () else do
            let bl1' = head bl1
            writeTVar (blocks game) ((bl1' {typePower = FastBall}) : tail bl1)
    return $ game 
    

-- | updates game state
update :: Float -> GameStatus -> IO (GameStatus)
update seconds game = do
    bl1 <- atomically $ readTVar $ blocks game
    if isPaused game then return game else do
        if (not $ hasBlocks bl1) then return $ game { gameStat = 1} else do
            if dropped 1 then return $ game { gameStat = (-1) } else do 
                x1 <- updateBall seconds game
                x2 <- updatePlayer seconds x1
                x3 <- updateWall seconds x2
                x4 <- updateBlocks seconds x3
                x5 <- updatePaddle seconds x4
                return x5
    where
        dropped 1  = y < (-halfHeight) - 5
        y          = snd $ ballLoc game

handleKeys :: Event -> GameStatus -> IO (GameStatus)
-- restarting game
handleKeys (EventKey (Char 'r') Down _ _) game = do
    atomically $ do
        writeTVar (blocks game) (map genBlock1 [0..39])
        return ()
    return ( game 
        { ballLoc   = (0, -100)
        , ballVel   = ballVelocity
        , playerLoc = 0
        , playerVel = 0
        , playerAcc = playerAcceleration
        , isPaused  = True
        , gameStat  = 0
        } )
-- Tecla 'p' pausa e despausa o jogo.
handleKeys (EventKey (Char 'p') Down _ _) game = return $ invPause game
-- Moving player to the left
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = return $ decVel game 1
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = return $ incVel game 1
-- Moving player to the right
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = return $ incVel game 1
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = return $ decVel game 1
-- ignore any other input
handleKeys _ game = return $ game

-- increments player velocity
incVel :: GameStatus -> Int -> GameStatus
incVel game 1 = game { playerVel = playerVel'}
    where
        playerVel' = playerVel game + playerAcc game

-- decrements player velocity
decVel :: GameStatus -> Int -> GameStatus
decVel game 1 = game { playerVel = playerVel' }
    where
        playerVel' = playerVel game - playerAcc game

-- invert pause state
invPause :: GameStatus -> GameStatus
invPause game = game { isPaused = isPaused' }
    where
        isPaused' = not $ isPaused game