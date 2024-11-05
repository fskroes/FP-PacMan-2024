module Controller.Input(
    handleKeyboardInput,
    isInButton
) where

import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Model.Types
import Controller.IO (saveGameState, loadGameState)

-- Check if mouse click is within button bounds
isInButton :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
isInButton (x, y) (bx, by, width, height) =
    x >= bx && x <= bx + width && y >= by && y <= by + height

-- handle keyboard input and mouse clicks
handleKeyboardInput :: Gloss.Event -> GameState -> IO GameState
handleKeyboardInput (Gloss.EventKey (Gloss.Char 'w') Gloss.Down _ _) gameState = 
    return $ gameState { pacman = (pacman gameState) { pacmanDirection = Down } }
handleKeyboardInput (Gloss.EventKey (Gloss.Char 's') Gloss.Down _ _) gameState = 
    return $ gameState { pacman = (pacman gameState) { pacmanDirection = Up } }
handleKeyboardInput (Gloss.EventKey (Gloss.Char 'a') Gloss.Down _ _) gameState = 
    return $ gameState { pacman = (pacman gameState) { pacmanDirection = LeftDir } }
handleKeyboardInput (Gloss.EventKey (Gloss.Char 'd') Gloss.Down _ _) gameState = 
    return $ gameState { pacman = (pacman gameState) { pacmanDirection = RightDir } }
handleKeyboardInput (Gloss.EventKey (Gloss.Char 'p') Gloss.Down _ _) gameState = 
    return $ gameState { 
        gameStatus = togglePause (gameStatus gameState),
        statusMessage = Nothing
    }
    where
        togglePause Paused = Ongoing
        togglePause Ongoing = Paused
        togglePause status = status

-- Handle mouse clicks for save/load buttons
handleKeyboardInput (Gloss.EventKey (Gloss.MouseButton Gloss.LeftButton) Gloss.Down _ (x, y)) gameState = 
    let saveButtonBounds = (180, -350, 100, 30)
        loadButtonBounds = (180, -400, 100, 30)
    in if isInButton (x, y) saveButtonBounds
       then do
           putStrLn "Saving game..."
           saveGameState gameState
           putStrLn "Game saved!"
           return $ gameState { 
               statusMessage = Just ("Game Saved!", 1.5)
           }
       else if isInButton (x, y) loadButtonBounds
       then do
           putStrLn "Loading game..."
           maybeLoadedState <- loadGameState
           case maybeLoadedState of
               Just loadedState -> do
                   putStrLn "Game loaded!"
                   return $ loadedState { 
                       statusMessage = Just ("Game Loaded!", 1.5)
                   }
               Nothing -> do
                   putStrLn "Error loading game!"
                   return $ gameState { 
                       statusMessage = Just ("Error: Could not load game!", 1.5)
                   }
       else return gameState

handleKeyboardInput _ gameState = return gameState