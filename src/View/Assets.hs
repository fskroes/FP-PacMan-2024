module View.Assets (
    windowWidth,
    windowHeight,
    windowDisplayMode,
    backgroundColor
) where 

import Graphics.Gloss

-- Define the window size
windowWidth :: Int
windowWidth = 600

windowHeight :: Int
windowHeight = 800

-- Define the window display mode
windowDisplayMode :: Display
windowDisplayMode = InWindow "Pacman" (windowWidth, windowHeight) (100, 100)

-- Define the background color
backgroundColor :: Color
backgroundColor = makeColorI 0 0 0 255
