module Model.Common(
    moveInDirection,
    changeDirection,
    isValidPosition,
    oppositeDirection,
    getDirectionFromPositions,
    moveTowards
) where

import Model.Types(Board(..), Cell(..), Position(..), Direction(..), MazeElement(..))

moveInDirection :: Position -> Direction -> Float -> Position
moveInDirection (Position (x, y)) direction moveAmount = 
    case direction of
        Up -> Position (fromIntegral (round x), y - moveAmount)
        Down -> Position (fromIntegral (round x), y + moveAmount)
        LeftDir -> Position (x - moveAmount, fromIntegral (round y))
        RightDir -> Position (x + moveAmount, fromIntegral (round y))

-- Helper to change direction when hitting a wall
changeDirection :: Direction -> Direction
changeDirection dir = case dir of
    Up -> Down
    Down -> Up
    LeftDir -> RightDir
    RightDir -> LeftDir

-- Check if a position is valid (not a wall)
isValidPosition :: Board -> Position -> Bool
isValidPosition board (Position (x, y)) = 
    let gridX = round x
        gridY = round y
        inBounds = gridX >= 0 && gridX < width board && 
                  gridY >= 0 && gridY < height board
    in inBounds && maze board !! gridY !! gridX /= Wall

-- Helper function to get opposite direction
oppositeDirection :: Direction -> Direction
oppositeDirection Up = Down
oppositeDirection Down = Up
oppositeDirection LeftDir = RightDir
oppositeDirection RightDir = LeftDir

-- Helper to determine direction from current to next position
getDirectionFromPositions :: Position -> Position -> Direction
getDirectionFromPositions (Position (x1, y1)) (Position (x2, y2))
    | x2 > x1 = RightDir
    | x2 < x1 = LeftDir
    | y2 > y1 = Down
    | otherwise = Up

-- Helper function to move towards a target
moveTowards :: Position -> Position -> Position
moveTowards (Position (x1, y1)) (Position (x2, y2)) =
    let dx = signum (x2 - x1) * 0.2  -- Smaller step size
        dy = signum (y2 - y1) * 0.2
        -- Round to grid when close to a grid position
        roundToGrid pos = if abs (pos - fromIntegral (round pos)) < 0.1
                         then fromIntegral (round pos)
                         else pos
        -- Only move in one direction at a time and align to grid
        (finalX, finalY) = if abs (x2 - x1) > abs (y2 - y1)
                          then (x1 + dx, roundToGrid y1)
                          else (roundToGrid x1, y1 + dy)
    in Position (finalX, finalY)