module Window where

import Graphics.Gloss

width :: Int
width = 421

height :: Int
height = 450

offset :: Int
offset = 100

background :: Color
background = black

window :: Display
window = InWindow "BreakOut" (width, height) (offset, offset)

halfWidth :: Float
halfWidth = fromIntegral width / 2

halfHeight :: Float
halfHeight = fromIntegral height / 2

wallColor :: Color
wallColor = blue

topWall :: Picture
topWall = translate 0 halfHeight
        $ color wallColor
        $ rectangleSolid (fromIntegral width) 10

leftWall :: Picture
leftWall = translate (-halfWidth) 0
         $ color wallColor
         $ rectangleSolid 10 (fromIntegral height)

rightWall :: Picture
rightWall = translate halfWidth 0
          $ color wallColor
          $ rectangleSolid 10 (fromIntegral height)

walls :: Picture
walls = pictures [leftWall, rightWall, topWall]

renderTxt :: Color -> String -> Picture
renderTxt col msg = translate (-100) 180 $ scale 0.2 0.2 $ color col $ Text msg

curMsg :: Int -> Bool -> Int-> Picture
curMsg (-1) paused _ = lostMsg 1
curMsg   1  paused 3 = winMsg 1
curMsg 0 True 1 = pauseMsg True
curMsg 0 paused 1 = winMsg 2
curMsg 0 paused 2 = winMsg 3
curMsg 0 paused 3 = winMsg 4
curMsg 0 paused  _ = pauseMsg paused

winMsg    1    = renderTxt green "You won! Press R to play again"
winMsg    2    = renderTxt green "Level 1"
winMsg    3    = renderTxt green "Level 2"
winMsg    4    = renderTxt green "Level 3"
lostMsg   1    = renderTxt red   "You lost!"
pauseMsg True  = renderTxt blue  "Press p to play!"
pauseMsg False = renderTxt blue  ""