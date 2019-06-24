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

--power up collision will be checked on game
--Another thread, that check existence of power up on screen to create another
powerUpMonitor :: TVar PowerUp -> IO ()
powerUpMonitor pu = do
    atomically $ do 
        pu1 <- readTVar pu
        -- == none, create new power up 
        if ((getPowerUpType pu1) == None) 
            then writeTVar pu (PUI (-50,-50) BigBar)
            else return()
    threadDelay 1000
    powerUpMonitor pu

        
main :: IO ()
main = do 
    bl1 <- atomically $ newTVar (map genBlock1 [0..39])
    pu <- atomically $ newTVar (initializePowerUps)
    let iState = initialState bl1 pu
    forkIO $ powerUpMonitor pu

    playIO window background fps iState render handleKeys update


