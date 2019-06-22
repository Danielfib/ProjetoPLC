-- | Jogo no estilo Breakout implementado em Haskell.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Window
import Game
import Blocks
import Control.Concurrent
import Control.Concurrent.STM

fps :: Int
fps = 60
        
main :: IO ()
main = do 
    bl1 <- atomically $ newTVar (map genBlock1 [0..39])

    let iState = initialState bl1
    
    playIO window background fps iState render handleKeys update
