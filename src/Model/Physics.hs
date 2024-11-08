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
    GhostHouseState(..),
    Speed(..),
    Score(..),
    Lives(..),
    GameStatus(..),
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
    heuristic
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
                Exiting -> handleExiting ghost
                Outside -> handleNormalMovement ghost gameState
        Frightened t -> handleFrightened ghost gameState
        Eaten -> handleEaten ghost gameState

handleInHouse :: Ghost -> Ghost
handleInHouse ghost =
    let pos = ghostPosition ghost
        newPos = moveInDirection pos Up (ghostSpeedValue $ ghostSpeed ghost)
    in ghost { ghostPosition = newPos }

handleExiting :: Ghost -> Ghost
handleExiting ghost =
    let exitPos = Position (14.0, 11.0)  -- Ghost house exit position
        currentPos = ghostPosition ghost
        dist = heuristic currentPos exitPos
    in if dist < 0.5
       then ghost { ghostHouseState = Outside }
       else moveGhostTowards ghost exitPos defaultGameState  -- Use a dummy gamestate for movement

-- Handle normal ghost movement outside the house
handleNormalMovement :: Ghost -> GameState -> Ghost
handleNormalMovement ghost gameState =
    let targetPos = getGhostTarget ghost (pacman gameState)
    in moveGhostTowards ghost targetPos gameState

-- Minimal game state for movement calculations
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