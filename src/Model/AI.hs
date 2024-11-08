module Model.AI(
    moveGhostTowards,
    handleFrightened,
    handleEaten,
    updateBlinky,
    updatePinky,
    updateClyde,
    updateInky,
    getGhostTarget,
    chooseDirection,
    getNewDirection,
    getAheadPosition,
    getFlankPosition,
    getRandomPatrolPosition,
    shouldExitHouse,
    getPinkyTarget,
    getClydeTarget,
    heuristic
) where

import Model.Types(
    Ghost(..),
    GhostType(..),
    Direction(..),
    Position(..),
    Speed(..),
    Node(..),
    Board,
    GameState(..),
    PacMan(..),
    GhostStatus(..),
    GhostHouseState(..)
    )
import Model.Common(
    isValidPosition,
    moveInDirection,
    oppositeDirection,
    getDirectionFromPositions,
    changeDirection
    )
import Init(getGhostStartPosition)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.List (minimumBy, maximumBy)
import Data.Bifunctor (bimap)
import System.Random (randomRIO)

-- Move ghost towards target position
moveGhostTowards :: Ghost -> Position -> Ghost
moveGhostTowards ghost targetPos =
    let Position (gx, gy) = ghostPosition ghost
        Position (tx, ty) = targetPos
        -- Calculate direction vector
        dx = tx - gx
        dy = ty - gy
        -- Normalize the movement
        len = sqrt (dx * dx + dy * dy)
        (nx, ny) = if len > 0
                   then (dx / len, dy / len)
                   else (0, 0)
        -- Apply movement with fixed step size
        stepSize = 0.5  -- Adjust this value as needed
        newPos = Position (gx + nx * stepSize, gy + ny * stepSize)
    in ghost { ghostPosition = newPos }

-- Handle frightened ghost behavior
handleFrightened :: Ghost -> GameState -> Ghost
handleFrightened ghost gameState =
    let currentPos = ghostPosition ghost
        pacPos = pacmanPosition (pacman gameState)
        moveAmount = 0.1  -- Slower movement when frightened
        
        -- Ensure ghost is at a valid position first
        validCurrentPos = if isValidPosition (board gameState) currentPos
                         then currentPos
                         else getGhostStartPosition (ghostType ghost)
        
        -- Get valid grid-aligned position
        Position (gx, gy) = validCurrentPos
        gridPos = Position (fromIntegral $ round gx, fromIntegral $ round gy)
        
        -- Only change direction at grid intersections
        isOnGrid = heuristic gridPos validCurrentPos < 0.1
        
        -- Get all valid moves from the grid position
        validDirections = filter 
            (\dir -> isValidPosition (board gameState) (moveInDirection gridPos dir))
            [Up, Down, LeftDir, RightDir]
        
        -- Remove opposite of current direction if possible
        allowedDirections = 
            let filtered = filter (/= oppositeDirection (ghostDirection ghost)) validDirections
            in if null filtered then validDirections else filtered
        
        -- Choose direction that maximizes distance from Pac-Man
        newDir = if isOnGrid && not (null allowedDirections)
                 then maximumBy 
                      (comparing (\dir -> 
                          heuristic (moveInDirection gridPos dir) pacPos))
                      allowedDirections
                 else ghostDirection ghost
        
        -- Move in the chosen direction with small step size
        newPos = moveInDirection validCurrentPos newDir
        finalPos = if isValidPosition (board gameState) newPos
                  then newPos
                  else validCurrentPos
    in ghost { 
        ghostPosition = finalPos,
        ghostDirection = newDir,
        ghostSpeed = Speed moveAmount
    }

-- Handle eaten ghost behavior
handleEaten :: Ghost -> GameState -> Ghost
handleEaten ghost gameState =
    let startPos = getGhostStartPosition (ghostType ghost)
        currentPos = ghostPosition ghost
        -- Force ghost into Eaten state if not already
        ghostWithEatenStatus = ghost { ghostStatus = Eaten }
        -- If at start position, transition to Chasing
        isAtStart = heuristic currentPos startPos < 0.5
        
        -- Simple direct movement towards start position
        moveTowardsStart = 
            let Position (gx, gy) = currentPos
                Position (sx, sy) = startPos
                dx = sx - gx
                dy = sy - gy
                len = sqrt (dx * dx + dy * dy)
                (nx, ny) = if len > 0
                          then (dx / len, dy / len)
                          else (0, 0)
                stepSize = 0.5
                newPos = Position (gx + nx * stepSize, gy + ny * stepSize)
                newDir = getDirectionFromPositions currentPos newPos
            in ghost { 
                ghostPosition = newPos,
                ghostDirection = newDir,
                ghostStatus = if isAtStart then Chasing else Eaten,
                ghostHouseState = if isAtStart then InHouse else Outside
            }
    in moveTowardsStart

getPinkyTarget :: PacMan -> Position
getPinkyTarget pacman =
    let Position (px, py) = pacmanPosition pacman
        offset = case pacmanDirection pacman of
            Up -> (-4, -4)      -- Includes the famous Pinky targeting bug
            Down -> (0, 4)
            LeftDir -> (-4, 0)
            RightDir -> (4, 0)
    in Position (bimap (px +) (py +) offset)

getInkyTarget :: Ghost -> PacMan -> Position
getInkyTarget ghost pacman =
    let Position (px, py) = pacmanPosition pacman
        Position (bx, by) = ghostPosition ghost
        -- Get position 2 tiles ahead of Pacman
        (tx, ty) = case pacmanDirection pacman of
            Up -> (px, py - 2)
            Down -> (px, py + 2)
            LeftDir -> (px - 2, py)
            RightDir -> (px + 2, py)
        -- Double the vector from Blinky to that position
        dx = tx - bx
        dy = ty - by
    in Position (tx + dx, ty + dy)

getClydeTarget :: Ghost -> PacMan -> Position
getClydeTarget ghost pacman =
    let pos@(Position (gx, gy)) = ghostPosition ghost
        pacPos@(Position (px, py)) = pacmanPosition pacman
        distance = heuristic pos pacPos
    in if distance > 8
        then pacPos  -- Chase mode
        else Position (0, 29)  -- Scatter mode - return to corner

-- Add these ghost update functions
updateBlinky :: Ghost -> PacMan -> Ghost
updateBlinky ghost pacman = 
    let targetPos = pacmanPosition pacman  -- Directly chase Pac-Man
    in moveGhostTowards ghost targetPos

updatePinky :: Ghost -> PacMan -> Ghost
updatePinky ghost pacman =
    let Position (px, py) = pacmanPosition pacman
        -- Target 4 tiles ahead of Pac-Man
        targetPos = case pacmanDirection pacman of
            Up -> Position (px, py - 4)
            Down -> Position (px, py + 4)
            LeftDir -> Position (px - 4, py)
            RightDir -> Position (px + 4, py)
    in moveGhostTowards ghost targetPos

-- Update Clyde's position (shy ghost - runs away when close)
updateClyde :: Ghost -> PacMan -> GameState -> Ghost
updateClyde ghost pacman gameState =
    let Position (gx, gy) = ghostPosition ghost
        Position (px, py) = pacmanPosition pacman
        distance = sqrt ((gx - px)^2 + (gy - py)^2)
        -- Calculate target position
        rawTarget = if distance < 8
            then Position (gx * 2 - px, gy * 2 - py)  -- Run away
            else Position (0, 29)                      -- Scatter to corner
        -- Validate target position
        validTarget = if isValidPosition (board gameState) rawTarget
                     then rawTarget
                     else head $ getValidNeighbors (board gameState) (ghostPosition ghost) 
    in moveGhostTowards ghost validTarget

-- Update Inky's position (uses Blinky's position to determine target)
updateInky :: Ghost -> PacMan -> GameState -> Ghost
updateInky ghost pacman gameState =
    let blinky = head [g | g <- ghosts gameState, ghostType g == Blinky]
        rawTarget = getInkyTarget blinky pacman
        validTarget = if isValidPosition (board gameState) rawTarget
                     then rawTarget
                     else head $ getValidNeighbors (board gameState) (ghostPosition ghost)
    in moveGhostTowards ghost validTarget

-- Update ghost target based on type
getGhostTarget :: Ghost -> PacMan -> Position
getGhostTarget ghost pacman =
    let target = case ghostType ghost of
            Blinky -> pacmanPosition pacman  -- Direct chase
            Pinky -> getPinkyTarget pacman   -- 4 tiles ahead
            Inky -> getInkyTarget ghost pacman  -- Complex targeting
            Clyde -> getClydeTarget ghost pacman  -- Scatter/Chase
    in target

-- A* pathfinding algorithm
findPath :: Board -> Position -> Position -> Maybe [Position]
findPath board start goal = 
    let initialNode = Node start 0 (heuristic start goal) Nothing
        openSet = Set.singleton initialNode
        closedSet = Set.empty
    in Just $ astar board goal openSet closedSet Map.empty

astar :: Board -> Position -> Set.Set Node -> Set.Set Node -> Map.Map Position Node -> [Position]
astar board goal openSet closedSet cameFrom
    | Set.null openSet = []
    | nodePosition current == goal = reverse $ reconstructPath current
    | otherwise = astar board goal openSet' closedSet' cameFrom'
    where
        current = Set.findMin openSet
        openSet' = Set.delete current openSet
        closedSet' = Set.insert current closedSet
        neighbors = getValidNeighbors board (nodePosition current)
        (openSet'', cameFrom') = foldr (processNeighbor current goal) (openSet', cameFrom) neighbors

processNeighbor :: Node -> Position -> Position -> (Set.Set Node, Map.Map Position Node) -> (Set.Set Node, Map.Map Position Node)
processNeighbor current goal neighbor (openSet, cameFrom) =
    let tentativeG = gScore current + 1
        neighborNode = Node neighbor tentativeG (heuristic neighbor goal) (Just current)
    in case Map.lookup neighbor cameFrom of
        Just existing -> 
            if tentativeG < gScore existing
                then (Set.insert neighborNode openSet, Map.insert neighbor neighborNode cameFrom)
                else (openSet, cameFrom)
        Nothing -> (Set.insert neighborNode openSet, Map.insert neighbor neighborNode cameFrom)

-- Get valid neighboring positions
getValidNeighbors :: Board -> Position -> [Position]
getValidNeighbors board (Position (x, y)) = 
    filter (isValidPosition board)
        [ Position (x + 1, y)
        , Position (x - 1, y)
        , Position (x, y + 1)
        , Position (x, y - 1)
        ]

-- Manhattan distance heuristic
heuristic :: Position -> Position -> Float
heuristic (Position (x1, y1)) (Position (x2, y2)) = 
    abs (x1 - x2) + abs (y1 - y2)

reconstructPath :: Node -> [Position]
reconstructPath node = 
    case parent node of
        Nothing -> [nodePosition node]
        Just p -> nodePosition node : reconstructPath p

-- New helper function to choose direction based on ghost type
chooseDirection :: Ghost -> GameState -> Direction
chooseDirection ghost gameState =
    let Position (gx, gy) = ghostPosition ghost
        Position (px, py) = pacmanPosition (pacman gameState)
        moveAmount = 0.2
        -- Get possible directions (excluding walls)
        possibleDirs = filter 
            (\dir -> isValidPosition (board gameState) (moveInDirection (ghostPosition ghost) dir))
            [Up, Down, LeftDir, RightDir]
        -- Remove opposite of current direction to prevent immediate reversal
        allowedDirs = filter (/= oppositeDirection (ghostDirection ghost)) possibleDirs
        -- If no allowed directions, allow reversal
        finalDirs = if null allowedDirs then possibleDirs else allowedDirs
        -- Choose direction based on ghost type
        targetPos = case ghostType ghost of
            Blinky -> Position (px, py)  -- Chase directly
            Pinky  -> case pacmanDirection (pacman gameState) of  -- Target ahead of Pac-Man
                Up -> Position (px, py - 4)
                Down -> Position (px, py + 4)
                LeftDir -> Position (px - 4, py)
                RightDir -> Position (px + 4, py)
            Inky   -> Position (px + 2, py + 2)  -- Flank
            Clyde  -> if distance > 8  -- Scatter when far
                     then Position (px, py)
                     else Position (0, 0)
        distance = sqrt ((px - gx)^2 + (py - gy)^2)
        -- Choose direction that minimizes distance to target
        directionScores = [(dir, scoreDirection dir targetPos) | dir <- finalDirs]
        scoreDirection dir (Position (tx, ty)) = case dir of
            Up -> abs (gx - tx) + abs ((gy - moveAmount) - ty)
            Down -> abs (gx - tx) + abs ((gy + moveAmount) - ty)
            LeftDir -> abs ((gx - moveAmount) - tx) + abs (gy - ty)
            RightDir -> abs ((gx + moveAmount) - tx) + abs (gy - ty)
    in case finalDirs of
        [] -> ghostDirection ghost  -- Keep current direction if no valid moves
        _  -> fst $ minimumBy (comparing snd) directionScores

-- Add helper function to determine new direction at intersections
getNewDirection :: Ghost -> Position -> Board -> Direction
getNewDirection ghost targetPos board =
    let Position (gx, gy) = ghostPosition ghost
        Position (tx, ty) = targetPos
        -- Get possible directions at current position
        possibleDirs = filter 
            (\dir -> isValidPosition board (moveInDirection (Position (fromIntegral (round gx), fromIntegral (round gy))) dir))
            [Up, Down, LeftDir, RightDir]
        -- Choose direction that minimizes distance to target
        distanceToTarget dir =
            let Position (nx, ny) = moveInDirection (Position (fromIntegral (round gx), fromIntegral (round gy))) dir
            in (nx - tx)^2 + (ny - ty)^2
    in case possibleDirs of
        [] -> ghostDirection ghost  -- Keep current direction if no valid moves
        dirs -> minimumBy (comparing distanceToTarget) dirs

-- Get position 4 tiles ahead of Pacman
getAheadPosition :: PacMan -> Int -> Position
getAheadPosition pacman tiles =
    let Position (px, py) = pacmanPosition pacman
        tileDistance = fromIntegral tiles
        -- Ensure exact tile distance by rounding non-movement coordinate
        (newX, newY) = case pacmanDirection pacman of
            Up    -> (fromIntegral (round px), py - tileDistance)
            Down  -> (fromIntegral (round px), py + tileDistance)
            LeftDir  -> (px - tileDistance, fromIntegral (round py))
            RightDir -> (px + tileDistance, fromIntegral (round py))
    in Position (newX, newY)

-- Get flanking position (Inky's behavior)
getFlankPosition :: PacMan -> Ghost -> Position
getFlankPosition pacman ghost =
    let Position (px, py) = pacmanPosition pacman
        Position (gx, gy) = ghostPosition ghost
        -- Calculate position that's opposite to Blinky's position
        dx = px - gx
        dy = py - gy
    in Position (px + dx, py + dy)

-- Random patrol movement for Clyde
getRandomPatrolPosition :: Ghost -> Board -> IO Position
getRandomPatrolPosition ghost board = do
    let Position (gx, gy) = ghostPosition ghost
        possibleMoves = filter (isValidPosition board) 
            [ Position (gx + 1, gy)
            , Position (gx - 1, gy)
            , Position (gx, gy + 1)
            , Position (gx, gy - 1)
            ]
    case possibleMoves of
        [] -> return $ ghostPosition ghost  -- If no valid moves, stay in place
        moves -> do
            index <- randomRIO (0, length moves - 1)
            return $ moves !! index

shouldExitHouse :: Ghost -> Bool
shouldExitHouse ghost = 
    case ghostType ghost of
        Blinky -> True  -- Blinky starts outside
        Pinky -> True   -- Pinky exits quickly
        Inky -> dotThreshold > 30  -- Inky exits after 30 dots eaten
        Clyde -> dotThreshold > 60  -- Clyde exits after 60 dots eaten
    where
        dotThreshold = 0  -- TODO: Track dots eaten in GameState

