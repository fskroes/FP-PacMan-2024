module Controller.GameLoop(
    updateGame
) where

import Model.Types(GameState(..), GameStatus(..), PacMan(..), Ghost(..), PathCache(..))
import Model.State(updateGameState)

-- Update game state with time delta
updateGame :: Float -> GameState -> IO GameState
updateGame dt gameState = do
    let updatedGhosts = map (updateGhostPathCache dt) (ghosts gameState)
        gameStateWithUpdatedGhosts = gameState { ghosts = updatedGhosts }
    
    -- Always update status message timer regardless of game state
    let updatedStatusMessage = case statusMessage gameStateWithUpdatedGhosts of
            Just (msg, timeLeft) -> 
                if timeLeft > dt
                then Just (msg, timeLeft - dt)
                else Nothing
            Nothing -> Nothing
    
    -- Update the game state based on current status
    case gameStatus gameStateWithUpdatedGhosts of
        Ongoing -> do
            -- Print positions for debugging
            putStrLn $ "PacMan pos: " ++ show (pacmanPosition $ pacman gameStateWithUpdatedGhosts)
            putStrLn $ "Ghost pos: " ++ show (map ghostPosition $ ghosts gameStateWithUpdatedGhosts)
            putStrLn $ "Status: " ++ show (statusMessage gameStateWithUpdatedGhosts)
            putStrLn "-------------------"
            
            -- Update game state with new status message
            let gameStateWithMessage = gameStateWithUpdatedGhosts { statusMessage = updatedStatusMessage }
            return $ updateGameState gameStateWithMessage
            
        Paused -> return $ gameStateWithUpdatedGhosts { statusMessage = updatedStatusMessage }
        _ -> return $ gameStateWithUpdatedGhosts { statusMessage = updatedStatusMessage }

-- Update this function
updateGhostPathCache :: Float -> Ghost -> Ghost
updateGhostPathCache dt ghost =
    case pathCache ghost of
        Nothing -> ghost
        Just cache -> ghost {
            pathCache = Just cache {
                updateTime = updateTime cache - dt  -- Now properly scoped
            }
        }
