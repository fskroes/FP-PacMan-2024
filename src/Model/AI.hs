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
    heuristic,
    findPath,
    limitStep
) where

import Model.Types
import Model.Common
import Init ( getGhostStartPosition )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.List (minimumBy, maximumBy, sortBy)
import Data.Bifunctor (bimap)
import System.Random (randomRIO)

-- Constants for path finding
pathUpdateInterval :: Float
pathUpdateInterval = 0.5 

-- Helper function to limit step size
limitStep :: Float -> Float -> Float -> Float
limitStep maxStep dx dy =
    let dist = sqrt (dx * dx + dy * dy)
        scale = if dist > maxStep then maxStep / dist else 1.0
    in scale

-- Modified moveGhostTowards to ensure valid positions
moveGhostTowards :: Ghost -> Position -> GameState -> Ghost
moveGhostTowards ghost target gameState =
    let currentPos = ensureValidPosition (board gameState) (ghostPosition ghost)
        validTarget = ensureValidPosition (board gameState) target
        maxStep = ghostSpeedValue (ghostSpeed ghost)
        
        -- Round positions to nearest grid point
        Position (gx, gy) = currentPos
        Position (tx, ty) = validTarget
        
        -- Calculate direction vector
        dx = tx - gx
        dy = ty - gy
        dist = sqrt (dx * dx + dy * dy)
        
        -- Normalize and scale movement, ensuring we don't exceed maxStep
        (nx, ny) = if dist <= 0.001
                   then (0, 0)
                   else let scale = min maxStep dist / dist
                        in (dx * scale, dy * scale)
        
        -- Calculate new position
        proposedPos = Position (gx + nx, gy + ny)
        
        -- Ensure the new position is valid and on the grid
        finalPos = if isValidPosition (board gameState) proposedPos
                  then snapToGrid proposedPos
                  else snapToGrid currentPos
                  
    in ghost { ghostPosition = finalPos }
  where
    ensureValidPosition board pos@(Position (x, y)) =
        snapToGrid $ Position (
            clamp 0 (fromIntegral (width board - 1)) x,
            clamp 0 (fromIntegral (height board - 1)) y
        )
    clamp low high x = max low (min high x)
    -- Helper to snap position to nearest grid point
    snapToGrid (Position (x, y)) = 
        Position (
            fromIntegral (round x) + (x - fromIntegral (round x)) * 0.5,
            fromIntegral (round y) + (y - fromIntegral (round y)) * 0.5
        )

-- Handle frightened ghost behavior
handleFrightened :: Ghost -> GameState -> Ghost
handleFrightened ghost gameState =
    let currentPos = ghostPosition ghost
        pacPos = pacmanPosition (pacman gameState)
        Speed baseSpeed = ghostSpeed ghost
        frightenedSpeed = Speed (baseSpeed * frightenedSpeedMultiplier)
        
        validCurrentPos = if isValidPosition (board gameState) currentPos
                         then currentPos
                         else getGhostStartPosition (ghostType ghost)
        
        Position (gx, gy) = validCurrentPos
        gridPos = Position (fromIntegral $ round gx, fromIntegral $ round gy)
        
        isOnGrid = heuristic gridPos validCurrentPos < 0.1
        
        validDirections = filter 
            (\dir -> isValidPosition (board gameState) (moveInDirection gridPos dir (ghostSpeedValue frightenedSpeed)))
            [Up, Down, LeftDir, RightDir]
        
        allowedDirections = 
            let filtered = filter (/= oppositeDirection (ghostDirection ghost)) validDirections
            in if null filtered then validDirections else filtered
        
        newDir = if isOnGrid && not (null allowedDirections)
                 then maximumBy 
                      (comparing (\dir -> 
                          heuristic (moveInDirection gridPos dir (ghostSpeedValue frightenedSpeed)) pacPos))
                      allowedDirections
                 else ghostDirection ghost
        
        newPos = moveInDirection validCurrentPos newDir (ghostSpeedValue frightenedSpeed)
        finalPos = if isValidPosition (board gameState) newPos
                  then newPos
                  else validCurrentPos
    in ghost { 
        ghostPosition = finalPos,
        ghostDirection = newDir,
        ghostSpeed = frightenedSpeed
    }

-- Handle eaten ghost behavior
handleEaten :: Ghost -> GameState -> Ghost
handleEaten ghost gameState =
    let startPos = getGhostStartPosition (ghostType ghost)
        currentPos = ghostPosition ghost
        isAtStart = heuristic currentPos startPos < 0.5
        
        -- Set speed for eaten ghost
        Speed currentSpeed = ghostSpeed ghost
        eatenSpeed = Speed (currentSpeed * eatenSpeedMultiplier)
        
        moveTowardsStart = 
            let Position (gx, gy) = currentPos
                Position (sx, sy) = startPos
                dx = sx - gx
                dy = sy - gy
                len = sqrt (dx * dx + dy * dy)
                (nx, ny) = if len > 0
                          then (dx / len, dy / len)
                          else (0, 0)
                newPos = Position (gx + nx * ghostSpeedValue eatenSpeed, gy + ny * ghostSpeedValue eatenSpeed)
                newDir = getDirectionFromPositions currentPos newPos
            in ghost { 
                ghostPosition = newPos,
                ghostDirection = newDir,
                ghostStatus = if isAtStart 
                             then Chasing 
                             else Eaten,
                ghostSpeed = if isAtStart 
                           then baseGhostSpeed  -- Reset to base speed when reaching home
                           else eatenSpeed,     -- Use faster speed while returning
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
updateBlinky :: Ghost -> PacMan -> GameState -> Ghost
updateBlinky ghost pacman gameState = 
    let targetPos = pacmanPosition pacman  -- Directly chase Pac-Man
    in moveGhostTowards ghost targetPos gameState

updatePinky :: Ghost -> PacMan -> GameState -> Ghost
updatePinky ghost pacman gameState =
    let Position (px, py) = pacmanPosition pacman
        -- Target 4 tiles ahead of Pac-Man
        targetPos = case pacmanDirection pacman of
            Up -> Position (px, py - 4)
            Down -> Position (px, py + 4)
            LeftDir -> Position (px - 4, py)
            RightDir -> Position (px + 4, py)
    in moveGhostTowards ghost targetPos gameState

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
    in moveGhostTowards ghost validTarget gameState

-- Update Inky's position (uses Blinky's position to determine target)
updateInky :: Ghost -> PacMan -> GameState -> Ghost
updateInky ghost pacman gameState =
    let blinky = head [g | g <- ghosts gameState, ghostType g == Blinky]
        rawTarget = getInkyTarget blinky pacman
        validTarget = if isValidPosition (board gameState) rawTarget
                     then rawTarget
                     else head $ getValidNeighbors (board gameState) (ghostPosition ghost)
    in moveGhostTowards ghost validTarget gameState

-- Update ghost target based on type
getGhostTarget :: Ghost -> PacMan -> Position
getGhostTarget ghost pacman =
    let target = case ghostType ghost of
            Blinky -> pacmanPosition pacman  -- Direct chase
            Pinky -> getPinkyTarget pacman   -- 4 tiles ahead
            Inky -> getInkyTarget ghost pacman  -- Complex targeting
            Clyde -> getClydeTarget ghost pacman  -- Scatter/Chase
    in target

-- Improved A* implementation with path smoothing
findPath :: Board -> Position -> Position -> Maybe [Position]
findPath board start goal = 
    let initialNode = Node start 0 (heuristic start goal) Nothing
        path = astar board goal (Set.singleton initialNode) Set.empty Map.empty
    in smoothPath board <$> path

-- Path smoothing to make movement more natural
smoothPath :: Board -> [Position] -> [Position]
smoothPath board path =
    case path of
        [] -> []
        [x] -> [x]
        (p1:p2:rest) ->
            if canMoveDirect board p1 p2
            then p1 : smoothPath board (p2:rest)
            else p1 : findIntermediatePoints p1 p2 ++ smoothPath board (p2:rest)

-- Check if direct movement between points is possible
canMoveDirect :: Board -> Position -> Position -> Bool
canMoveDirect board (Position (x1, y1)) (Position (x2, y2)) =
    let steps = max (abs (x2 - x1)) (abs (y2 - y1))
        dx = (x2 - x1) / steps
        dy = (y2 - y1) / steps
        positions = [Position (x1 + dx * i, y1 + dy * i) | i <- [0..steps]]
    in all (isValidPosition board) positions

-- Generate intermediate points for smoother movement
findIntermediatePoints :: Position -> Position -> [Position]
findIntermediatePoints (Position (x1, y1)) (Position (x2, y2)) =
    let dx = (x2 - x1) / 4  -- Divide distance into 4 steps
        dy = (y2 - y1) / 4
    in [Position (x1 + dx * i, y1 + dy * i) | i <- [1..3]]

-- Helper function for A* pathfinding
processNeighbor :: Node -> Position -> Position -> (Set.Set Node, Map.Map Position Node) -> (Set.Set Node, Map.Map Position Node)
processNeighbor current goal neighbor (openSet, cameFrom) =
    let tentativeG = gScore current + heuristic (nodePosition current) neighbor
        neighborNode = Node neighbor tentativeG (heuristic neighbor goal) (Just current)
    in case Map.lookup neighbor cameFrom of
        Nothing -> 
            (Set.insert neighborNode openSet, Map.insert neighbor neighborNode cameFrom)
        Just existing ->
            if tentativeG < gScore existing
            then 
                (Set.insert neighborNode $ Set.delete existing openSet, 
                 Map.insert neighbor neighborNode cameFrom)
            else 
                (openSet, cameFrom)

-- More accurate A* implementation
astar :: Board -> Position -> Set.Set Node -> Set.Set Node -> Map.Map Position Node -> Maybe [Position]
astar board goal openSet closedSet cameFrom
    | Set.null openSet = Nothing
    | nodePosition current == goal = Just $ reverse $ reconstructPath current
    | otherwise = astar board goal openSet' closedSet' cameFrom'
    where
        current = Set.findMin openSet
        openSet' = Set.delete current openSet
        closedSet' = Set.insert current closedSet
        neighbors = getNeighbors board (nodePosition current)
        (openSet'', cameFrom') = foldr (processNeighbor current goal) (openSet', cameFrom) neighbors

-- Get valid neighboring positions with diagonal movement
getNeighbors :: Board -> Position -> [Position]
getNeighbors board (Position (x, y)) = 
    filter (isValidPosition board)
        [ Position (x + dx, y + dy) 
        | dx <- [-1, 0, 1]
        , dy <- [-1, 0, 1]
        , dx /= 0 || dy /= 0  -- Exclude current position
        , abs dx + abs dy <= 2  -- Allow diagonal but prefer cardinal directions
        ]

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
        Speed speed = ghostSpeed ghost
        -- Get possible directions (excluding walls)
        possibleDirs = filter 
            (\dir -> isValidPosition (board gameState) (moveInDirection (ghostPosition ghost) dir speed))
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
                LeftDir  -> Position (px - 4, py)
                RightDir -> Position (px + 4, py)
            Inky   -> Position (px + 2, py + 2)  -- Flank
            Clyde  -> if distance > 8  -- Scatter when far
                     then Position (px, py)
                     else Position (0, 0)
        distance = sqrt ((px - gx)^2 + (py - gy)^2)
        -- Choose direction that minimizes distance to target
        directionScores = [(dir, scoreDirection dir targetPos) | dir <- finalDirs]
        scoreDirection dir (Position (tx, ty)) = case dir of
            Up -> abs (gx - tx) + abs ((gy - speed) - ty)
            Down -> abs (gx - tx) + abs ((gy + speed) - ty)
            LeftDir -> abs ((gx - speed) - tx) + abs (gy - ty)
            RightDir -> abs ((gx + speed) - tx) + abs (gy - ty)
    in case finalDirs of
        [] -> ghostDirection ghost  -- Keep current direction if no valid moves
        _  -> fst $ minimumBy (comparing snd) directionScores

-- Add helper function to determine new direction at intersections
getNewDirection :: Ghost -> Position -> Board -> Direction  
getNewDirection ghost targetPos board =
    let Position (gx, gy) = ghostPosition ghost
        Position (tx, ty) = targetPos
        Speed speed = ghostSpeed ghost
        gridPos = Position (fromIntegral (round gx), fromIntegral (round gy))
        -- Get possible directions at current position
        possibleDirs = filter 
            (\dir -> isValidPosition board (moveInDirection gridPos dir speed))
            [Up, Down, LeftDir, RightDir]
        -- Choose direction that minimizes distance to target
        distanceToTarget dir =
            let Position (nx, ny) = moveInDirection gridPos dir speed
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

shouldExitHouse :: Ghost -> GameState -> Bool
shouldExitHouse ghost gameState = 
    case ghostType ghost of
        Blinky -> True
        Pinky -> dotsEaten gameState >= 0  -- Exit immediately
        Inky -> dotsEaten gameState >= 30
        Clyde -> dotsEaten gameState >= 60

