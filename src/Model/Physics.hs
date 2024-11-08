module Model.Physics(
    moveGhost,
    movePacMan,
    isColliding,
    handleExiting,
    handleInHouse
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
    GhostHouseState(..),
    Speed(..),
    Score(..),
    Lives(..),
    GameStatus(..),
    GhostType(..),
    ghostSpeedValue
    )
import Model.Common(
    isValidPosition,
    moveInDirection,
    )
import Model.AI(
    getGhostTarget,
    handleFrightened,
    handleEaten,
    moveGhostTowards,
    shouldExitHouse,
    heuristic,
    updateBlinky,
    updatePinky,
    updateInky,
    updateClyde
    )
import Init(initialBoard)

-- Move ghost based on AI Pathfinding algorithm
moveGhost :: Ghost -> GameState -> Ghost
moveGhost ghost gameState = 
    case ghostStatus ghost of
        Chasing -> 
            case ghostHouseState ghost of
                InHouse -> 
                    if shouldExitHouse ghost gameState
                    then ghost { ghostHouseState = Exiting }
                    else handleInHouse ghost
                Exiting -> handleExiting ghost {ghostSpeed = Speed 0.2}  -- Add speed to ensure movement
                Outside -> 
                    case ghostType ghost of
                        Blinky -> updateBlinky ghost (pacman gameState) gameState
                        Pinky -> updatePinky ghost (pacman gameState) gameState
                        Inky -> updateInky ghost (pacman gameState) gameState  -- Add pacman parameter
                        Clyde -> updateClyde ghost (pacman gameState) gameState
        Frightened t -> handleFrightened ghost gameState
        Eaten -> handleEaten ghost gameState

handleInHouse :: Ghost -> Ghost
handleInHouse ghost =
    let pos = ghostPosition ghost
        Speed speed = ghostSpeed ghost
        newPos = moveInDirection pos Up speed  -- Use actual speed value
    in ghost { 
        ghostPosition = newPos,
        ghostDirection = Up
    }

handleExiting :: Ghost -> Ghost
handleExiting ghost =
    let exitPos = Position (14.0, 11.0)  -- Ghost house exit position
        currentPos = ghostPosition ghost
        Speed speed = ghostSpeed ghost
        dist = heuristic currentPos exitPos
        
        -- Move towards exit
        dx = (fst (getCoords exitPos) - fst (getCoords currentPos))
        dy = (snd (getCoords exitPos) - snd (getCoords currentPos))
        len = sqrt (dx * dx + dy * dy)
        
        newPos = if len > 0
                 then let scale = speed / len
                      in Position (fst (getCoords currentPos) + dx * scale,
                                 snd (getCoords currentPos) + dy * scale)
                 else currentPos
                 
    in if dist < 0.5
       then ghost { 
           ghostHouseState = Outside,
           ghostDirection = Up,
           ghostPosition = exitPos  -- Ensure clean exit position
       }
       else ghost {
           ghostPosition = newPos,
           ghostDirection = Up
       }
  where
    getCoords (Position (x, y)) = (x, y)

-- Handle normal ghost movement outside the house
handleNormalMovement :: Ghost -> GameState -> Ghost
handleNormalMovement ghost gameState =
    let targetPos = getGhostTarget ghost (pacman gameState)
        -- Ensure target is within board bounds
        Position (tx, ty) = targetPos
        boundedTarget = Position (
            clamp 0 (fromIntegral (width (board gameState) - 1)) tx,
            clamp 0 (fromIntegral (height (board gameState) - 1)) ty
            )
    in moveGhostTowards ghost boundedTarget gameState
  where
    clamp low high x = max low (min high x)

-- Add this helper function
isColliding :: Position -> Position -> Bool
isColliding (Position (x1, y1)) (Position (x2, y2)) =
    let dx = x1 - x2
        dy = y1 - y2
        threshold = 0.5 
    in dx * dx + dy * dy < threshold * threshold

movePacMan :: PacMan -> Direction -> Board -> PacMan
movePacMan pacman dir board = 
    let Position (px, py) = pacmanPosition pacman
        -- Only allow movement when at a grid position or continuing in same direction
        isOnGrid = abs (px - fromIntegral (round px)) < 0.1 && 
                   abs (py - fromIntegral (round py)) < 0.1
        moveAmount = 0.2
        -- Snap to grid when changing direction
        snappedPos = Position (fromIntegral (round px), fromIntegral (round py))
        startPos = if dir /= pacmanDirection pacman && isOnGrid
                  then snappedPos
                  else pacmanPosition pacman
        Position (sx, sy) = startPos
        
        newPos = case dir of
            Up -> Position (fromIntegral (round sx), sy - moveAmount)
            Down -> Position (fromIntegral (round sx), sy + moveAmount)
            LeftDir -> Position (sx - moveAmount, fromIntegral (round sy))
            RightDir -> Position (sx + moveAmount, fromIntegral (round sy))

        finalPos = if isValidPosition board newPos
                  then newPos
                  else pacmanPosition pacman
                  
    in pacman { 
        pacmanPosition = finalPos,
        pacmanDirection = if isOnGrid && isValidPosition board newPos
                         then dir 
                         else pacmanDirection pacman
    }

-- Add default game state for movement
defaultGameState :: GameState
defaultGameState = GameState {
    board = initialBoard,
    pacman = PacMan (Position (0, 0)) Up (Speed 0),
    ghosts = [],
    dots = [],
    powerUps = [],
    activeEffects = [],
    score = Score 0,
    lives = Lives 3,
    gameStatus = Ongoing,
    powerPellets = [],
    statusMessage = Nothing,
    dotsEaten = 0
}