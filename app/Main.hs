module Main (main) where

import Controller.IO (saveGameState, loadGameState)
import Controller.GameLoop (updateGame)
import Controller.Input (handleKeyboardInput)
import View.Rendering (renderGameState)
import View.Assets (windowDisplayMode, backgroundColor)
import Init (initialGameState)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO 
    windowDisplayMode                   -- Window configuration
    backgroundColor                     -- Background color
    60                                  -- Frames per second
    initialGameState                    -- Initial game state
    renderGameState                     -- render :: GameState -> IO Picture
    handleKeyboardInput                 -- handleInput :: Event -> GameState -> IO GameState
    updateGame                          -- updateState :: Float -> GameState -> IO GameState
