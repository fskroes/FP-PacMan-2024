module Model.Physics(
    moveGhost,
    movePacMan,
    isColliding
) where

import Model.Types(
    Ghost(..),
    PacMan(..),
    GameState(..),
    Direction(..),
    Position(..),
    Time(..),
    GhostStatus(..),
    Board(..),
    )
import Model.Common(
    moveInDirection,
    isValidPosition,
    changeDirection
    )
import Model.AI(
    chooseDirection,
    getGhostTarget,
    handleFrightened,
    handleEaten
    )

-- Move ghost based on AI (to implement with pathfinding algorithm)
moveGhost :: Ghost -> GameState -> Ghost
moveGhost ghost gameState = 
    case ghostStatus ghost of
        Chasing -> 
            let currentPos = ghostPosition ghost
                targetPos = getGhostTarget ghost (pacman gameState)
                newDir = chooseDirection ghost gameState
                moveAmount = 0.2
                newPos = moveInDirection currentPos newDir
                finalPos = if isValidPosition (board gameState) newPos
                          then newPos
                          else currentPos
            in ghost { 
                ghostPosition = finalPos,
                ghostDirection = if finalPos == currentPos 
                               then changeDirection newDir
                               else newDir
            }
        Frightened (Time t) -> 
            let frightenedGhost = handleFrightened ghost gameState
            in frightenedGhost { 
                ghostStatus = Frightened (Time (t - 0.016))  -- Update timer
            }
        Eaten -> handleEaten ghost gameState

-- Add this helper function
isColliding :: Position -> Position -> Bool
isColliding (Position (x1, y1)) (Position (x2, y2)) =
    let dx = x1 - x2
        dy = y1 - y2
        threshold = 0.5  -- Adjust this value as needed
    in dx * dx + dy * dy < threshold * threshold

movePacMan :: PacMan -> Direction -> Board -> PacMan
movePacMan pacman dir board = 
    let Position (px, py) = pacmanPosition pacman
        -- Only allow movement when at a grid position
        isOnGrid = abs (px - fromIntegral (round px)) < 0.1 && 
                   abs (py - fromIntegral (round py)) < 0.1
        moveAmount = 0.2
        newPos = if isOnGrid
                 then case dir of
                    Up -> Position (fromIntegral (round px), py - moveAmount)
                    Down -> Position (fromIntegral (round px), py + moveAmount)
                    LeftDir -> Position (px - moveAmount, fromIntegral (round py))
                    RightDir -> Position (px + moveAmount, fromIntegral (round py))
                 else case pacmanDirection pacman of
                    Up -> Position (fromIntegral (round px), py - moveAmount)
                    Down -> Position (fromIntegral (round px), py + moveAmount)
                    LeftDir -> Position (px - moveAmount, fromIntegral (round py))
                    RightDir -> Position (px + moveAmount, fromIntegral (round py))
        finalPos = if isValidPosition board newPos
                  then newPos
                  else pacmanPosition pacman
    in pacman { 
        pacmanPosition = finalPos, 
        pacmanDirection = if isOnGrid then dir else pacmanDirection pacman
    }