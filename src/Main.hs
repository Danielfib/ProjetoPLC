module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Window
import Game
import Blocks
import PowerUp
import Control.Monad.Random
import Control.Concurrent
import Control.Concurrent.STM

fps :: Int
fps = 60

--resets power up (another) on random X
powerUpMonitor :: TVar PowerUp -> TVar Bool -> TVar Bool -> IO ()
powerUpMonitor pu flag newFlag = do
    --print("olha o powerup vindo ae")
    atomically $ do         
        writeTVar newFlag True
        spawnFlag <- readTVar flag
        if spawnFlag 
            then writeTVar pu (PUI (-100, 200) FastBall)
            else writeTVar pu (PUI (100, 200) SlowBall)
        writeTVar flag (not spawnFlag)
    threadDelay 15000000
    powerUpMonitor pu flag newFlag

        
main :: IO ()
main = do 
    powerUpSpawnFlag <- atomically $ newTVar (False)
    newPowerUpFlag <- atomically $ newTVar (True)

    bl1 <- atomically $ newTVar (map genBlock1 [0..29])
    pu <- atomically $ newTVar (initializePowerUps)

    let iState = initialState bl1 pu newPowerUpFlag
    
    forkIO $ powerUpMonitor pu powerUpSpawnFlag newPowerUpFlag

    playIO window background fps iState render handleKeys update


