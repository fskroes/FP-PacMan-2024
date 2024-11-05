module Controller.GameLoop(
    updateGame
) where

import Model.Types(GameState(..), GameStatus(..), PacMan(..), Ghost(..))
import Model.State(updateGameState)

-- Update game state with time delta
updateGame :: Float -> GameState -> IO GameState
updateGame dt gameState = do
    -- Always update status message timer regardless of game state
    let updatedStatusMessage = case statusMessage gameState of
            Just (msg, timeLeft) -> 
                if timeLeft > dt
                then Just (msg, timeLeft - dt)
                else Nothing
            Nothing -> Nothing
    
    -- Update the game state based on current status
    case gameStatus gameState of
        Ongoing -> do
            -- Print positions for debugging
            putStrLn $ "PacMan pos: " ++ show (pacmanPosition $ pacman gameState)
            putStrLn $ "Ghost pos: " ++ show (map ghostPosition $ ghosts gameState)
            putStrLn $ "Status: " ++ show (statusMessage gameState)
            putStrLn "-------------------"
            
            -- Update game state with new status message
            let gameStateWithMessage = gameState { statusMessage = updatedStatusMessage }
            return $ updateGameState gameStateWithMessage
            
        Paused -> return $ gameState { statusMessage = updatedStatusMessage }
        _ -> return $ gameState { statusMessage = updatedStatusMessage }
