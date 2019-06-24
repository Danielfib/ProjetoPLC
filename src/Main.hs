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
powerUpMonitor :: TVar PowerUp -> TVar Bool-> IO ()
powerUpMonitor pu flag = do
    print("olha o powerup vindo ae")
    atomically $ do         
        spawnFlag <- readTVar flag
        if spawnFlag 
            then writeTVar pu (PUI (-100,-50) BigBar)
            else writeTVar pu (PUI (100,-50) BigBar)
        writeTVar flag (not spawnFlag)
    threadDelay 10000000
    powerUpMonitor pu flag

        
main :: IO ()
main = do 
    powerUpSpawnFlag <- atomically $ newTVar (False)

    bl1 <- atomically $ newTVar (map genBlock1 [0..29])
    pu <- atomically $ newTVar (initializePowerUps)
    let iState = initialState bl1 pu
    
    forkIO $ powerUpMonitor pu powerUpSpawnFlag

    playIO window background fps iState render handleKeys update


