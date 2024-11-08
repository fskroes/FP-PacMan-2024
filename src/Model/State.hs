module Model.State(
    updateGameState,
    checkCollisions,
    handleCollision,
    checkDotCollision,
    isDotEaten,
    resetAfterDeath,
    updateGhostStatus
) where

import Model.Types(
    GameState(..),
    PacMan(..),
    Ghost(..),
    Dot(..),
    Score(..),
    Position(..),
    Direction(..),
    Lives(..),
    Ghost(..),
    PacMan(..),
    GhostType(..),
    GhostStatus(..),
    GhostHouseState(..),
    GameStatus(..),
    )

import Init(getGhostStartPosition)
import Model.PowerUps(
    updatePowerUps,
    checkPowerUpCollision
    )
import Model.Physics(
    moveGhost,
    movePacMan,
    isColliding
    )

-- Update updateGameState to handle power-up effects
updateGameState :: GameState -> GameState
updateGameState gameState = 
    let -- Move PacMan in current direction
        newPacman = movePacMan (pacman gameState) 
                             (pacmanDirection (pacman gameState)) 
                             (board gameState)
        
        -- Update power-up effects
        gameStateWithEffects = updatePowerUps 0.016 gameState
        
        -- Update ghosts one at a time to maintain consistent state
        newGhosts = foldl (\ghosts g -> 
            let updatedGhost = moveGhost g gameStateWithEffects { ghosts = ghosts }
            in updatedGhost : filter (\og -> ghostType og /= ghostType g) ghosts
            ) [] (ghosts gameStateWithEffects)
        
        -- Update dots and score
        newDots = filter (not . isDotEaten newPacman) (dots gameState)
        dotsEatenCount = length (dots gameState) - length newDots
        newScore = Score $ case score gameState of
            Score s -> s + (dotsEatenCount * 10)
            
        newState = gameStateWithEffects {
            pacman = newPacman,
            ghosts = reverse newGhosts,  -- Reverse to maintain original order
            dots = newDots,
            score = newScore,
            dotsEaten = dotsEaten gameState + dotsEatenCount
        }
    in checkCollisions newState

-- Check for collisions between Pacman and all game entities
checkCollisions :: GameState -> GameState
checkCollisions gameState =
    let pacman' = pacman gameState
        -- Check ghost collisions
        gameState' = foldr (handleCollision pacman') gameState (ghosts gameState)
        -- Check dot collisions
        gameState'' = foldr (checkDotCollision pacman') gameState' (dots gameState)
        -- Check power-up collisions
        gameState''' = foldr (checkPowerUpCollision pacman') gameState'' (powerUps gameState)
    in gameState'''

-- Handle collision between pacman and ghosts
handleCollision :: PacMan -> Ghost -> GameState -> GameState
handleCollision pacman ghost gameState = 
    if isColliding (pacmanPosition pacman) (ghostPosition ghost)
        then case ghostStatus ghost of
            Frightened _ -> gameState { 
                score = Score (currentScore + 200),
                ghosts = map (\g -> 
                    if ghostPosition g == ghostPosition ghost
                    then g { 
                        ghostStatus = Eaten,
                        ghostHouseState = InHouse,
                        ghostPosition = getGhostStartPosition (ghostType g),
                        ghostDirection = Up
                    }
                    else g
                ) (ghosts gameState)
            }
            _ -> let Lives currentLives = lives gameState
                 in if currentLives <= 1
                    then gameState { 
                        lives = Lives 0,
                        gameStatus = Loss 
                    }
                    else resetAfterDeath gameState  -- Reset positions but keep score
        else gameState
    where
        Score currentScore = score gameState

-- create function that checks if pacman has eaten a dot
checkDotCollision :: PacMan -> Dot -> GameState -> GameState
checkDotCollision pacman dot gameState = 
    if pacmanPosition pacman == Position (dotPosition dot)
        then gameState { 
            score = Score (currentScore + 10),
            dotsEaten = dotsEaten gameState + 1
        }
        else gameState
    where
        Score currentScore = score gameState

isDotEaten :: PacMan -> Dot -> Bool
isDotEaten pac (Dot (dx, dy)) =
    let Position (px, py) = pacmanPosition pac
        -- Use grid positions for comparison
        gridX = round px
        gridY = round py
    in fromIntegral gridX == dx && fromIntegral gridY == dy

-- Add this new function to reset positions after death
resetAfterDeath :: GameState -> GameState
resetAfterDeath gameState = gameState {
    -- Decrease lives by 1
    lives = let Lives l = lives gameState in Lives (l - 1),
    -- Reset Pac-Man to starting position
    pacman = (pacman gameState) {
        pacmanPosition = Position (14.0, 23.0),
        pacmanDirection = LeftDir
    },
    -- Reset ghosts to their starting positions
    ghosts = map (\ghost -> ghost {
        ghostPosition = getGhostStartPosition (ghostType ghost),
        ghostDirection = Up,
        ghostStatus = Chasing,
        ghostHouseState = case ghostType ghost of
            Blinky -> Outside  -- Blinky starts outside
            _ -> InHouse,      -- Others start inside
        pathCache = Nothing  -- Reset path cache
    }) (ghosts gameState),
    -- Keep the game going
    gameStatus = Ongoing
}

-- Update the status of a specific ghost in the list of ghosts
updateGhostStatus :: Ghost -> [Ghost] -> [Ghost]
updateGhostStatus targetGhost = map (\ghost -> 
    if ghostPosition ghost == ghostPosition targetGhost
        then ghost { ghostStatus = Eaten }
        else ghost)