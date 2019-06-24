module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Window
import Game
import Blocks
import PowerUp
import Control.Concurrent
import Control.Concurrent.STM

fps :: Int
fps = 60

--resets power up (another) on random X
powerUpMonitor :: TVar PowerUp -> IO ()
powerUpMonitor pu = do
    print("olha o powerup vindo ae")
    atomically $ do 
        pu1 <- readTVar pu
        writeTVar pu (PUI (-50,-50) BigBar)
    threadDelay 10000000
    powerUpMonitor pu

        
main :: IO ()
main = do 
    bl1 <- atomically $ newTVar (map genBlock1 [0..39])
    pu <- atomically $ newTVar (initializePowerUps)

    let iState = initialState bl1 pu
    forkIO $ powerUpMonitor pu

    playIO window background fps iState render handleKeys update
