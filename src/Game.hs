module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Concurrent.STM
import Window
import Ball
import Blocks
import Player
import Collision
import PowerUp
import Data.Typeable

data GameStatus = Game
    { ballLoc   :: Position -- (x, y) ball coord
    , ballVel   :: Position -- (x, y) ball velocity
    , playerLoc :: Float    -- player X location
    , playerVel :: Float    -- player velocity
    , playerAcc :: Float    -- player acceleration
    , isPaused  :: Bool     
    , blocks    :: TVar Blocks   -- blocks on screen
    , powerUp  :: TVar PowerUp
    , gameStat  :: Int      -- game status: 0 - in game, 1 = victory, -1 = loss.  
    , gameLevel :: Int -- Current level of the game  
    }

-- initial game state
initialState :: TVar Blocks -> TVar PowerUp -> GameStatus
initialState b1 pu = Game
    { ballLoc   = (0 , -100)
    , ballVel   = ballVelocity
    , playerLoc = 0
    , playerVel = 0
    , playerAcc = playerAcceleration
    , isPaused  = True
    , blocks    = b1
    , powerUp   = pu
    , gameStat  = 0
    , gameLevel = 1
    }

render :: GameStatus -> IO (Picture)
render game = do
    bl1 <- atomically $ readTVar $ blocks game
    powerUpAux <- atomically $ readTVar $ powerUp game
    return (pictures [ballPics, walls, playerPics, pictures[drawBlocks bl1], msgPic, pictures [ powerUpBall $ (getPowerUpLocation(powerUpAux)) ]])
    where
        ballPics   = pictures [ ball $ ballLoc game ]
        playerPics = pictures [ mkPlayer $ playerLoc game ]
        msgPic     = curMsg (gameStat game) (isPaused game) (gameLevel game)

updateBall :: Float -> GameStatus -> IO (GameStatus)
updateBall seconds game = return $ game { ballLoc = moveBall seconds pos v }
    where pos = ballLoc game
          v   = ballVel game

updatePowerUp :: Float -> GameStatus -> IO (GameStatus)
updatePowerUp seconds game = do
    aux <- atomically $ readTVar $ powerUp game
    pickedUpPowerUP seconds (playerLoc game) aux
    --newPosition <- movePowerUp seconds aux
    atomically $ writeTVar (powerUp game) (PUI (movePowerUp seconds aux) (getPowerUpType aux))
    return $ game 
          
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
    ((v1x, v1y)) <- atomically $ do
        bl1 <- readTVar $ blocks game
        let (ballVel', p1) = blockCollision seconds bv bp bl1
        writeTVar (blocks game) (removeBlocks seconds bl1 bp bv)
        return (ballVel', p1)
    
    let v1 = (v1x, v1y)

    return $ game { ballVel = v1}
        where -- update ball velocity when hit block
            bv = ballVel game
            bp = ballLoc game
    

-- | updates game state
update :: Float -> GameStatus -> IO (GameStatus)
update seconds game = do
    bl1 <- atomically $ readTVar $ blocks game
    --printGameLevel game
    if isPaused game then return game else do
        if (not $ hasBlocks bl1) then 
            --return $ game { gameStat = 1, gameLevel=succ(gameLevel game), isPaused=True} 
            nextState game (gameLevel game)
        else do
            if dropped 1 then return $ game { gameStat = (-1) } else do 
                x1 <- updateBall seconds game
                x2 <- updatePlayer seconds x1
                x3 <- updateWall seconds x2
                x4 <- updateBlocks seconds x3
                x5 <- updatePaddle seconds x4
                x6 <- updatePowerUp seconds x5
                return x6
    where
        dropped 1  = y < (-halfHeight) - 5
        y          = snd $ ballLoc game
nextState :: GameStatus -> Int -> IO(GameStatus)
nextState game currentLevel= do
    if (currentLevel==1) then do
        atomically $ do
            writeTVar (blocks game) (map genBlock1 [0..59])
            return ()
        return ( game 
            { ballLoc   = (0, -100)
            , ballVel   = ballVelocity
            , playerLoc = 0
            , playerVel = 0
            , playerAcc = playerAcceleration
            , isPaused  = True
            , gameStat  = 0
            , gameLevel = succ(currentLevel)
            } )
    else do
        atomically $ do
            writeTVar (blocks game) (map genBlock1 [0..59])
            return ()
        return ( game 
            { ballLoc   = (0, -100)
            , ballVel   = ballVelocity2
            , playerLoc = 0
            , playerVel = 0
            , playerAcc = playerAcceleration
            , isPaused  = True
            , gameStat  = 0
            , gameLevel = succ(currentLevel)
            } )


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
        , gameLevel = 1
        } )
-- Tecla 'p' pausa e despausa o jogo.
handleKeys (EventKey (Char 'p') Down _ _) game = return $ invPause game
handleKeys (EventKey (Char 'n') Down _ _) game = nextState game (gameLevel game)
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

--applyPowerUp :: PowerUp -> ()
--applyPowerUp pu = do
