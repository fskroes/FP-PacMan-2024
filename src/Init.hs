module Init(
    initialGameState,
    initialBoard,
    initialPacMan,
    initialGhosts,
    initialDots,
    initialPowerUps,
    initialWalls,
    initialPowerPellets,
    getGhostStartPosition
) where

import Model.Types
import View.Rendering (generateDots, convertWallsToMaze)

-- Define the initial game state
initialGameState :: GameState
initialGameState = GameState
    { board = initialBoard
    , pacman = PacMan (Position (14.0, 23.0)) LeftDir (Speed 0.2)
    , ghosts = [ Ghost Blinky (Position (14.0, 11.0)) RightDir Chasing (Speed 0.2) Outside
               , Ghost Pinky  (Position (14.0, 14.0)) Up Chasing (Speed 0.2) InHouse
               , Ghost Inky   (Position (12.0, 14.0)) Up Chasing (Speed 0.2) InHouse
               , Ghost Clyde  (Position (16.0, 14.0)) Up Chasing (Speed 0.2) InHouse
               ]
    , dots = generateDots initialWalls
    , powerUps = initialPowerUps
    , activeEffects = []
    , score = Score 0
    , lives = Lives 3
    , gameStatus = Ongoing
    , powerPellets = initialPowerPellets
    , statusMessage = Nothing
    }

initialBoard :: Board
initialBoard = Board {
    maze = convertWallsToMaze initialWalls,
    width = 30,
    height = 30
}

initialPacMan :: PacMan
initialPacMan = PacMan {
    pacmanPosition = Position (14, 23),  -- Starting position
    pacmanDirection = LeftDir,
    pacmanSpeed = Speed 0.2
}

initialGhosts :: [Ghost]
initialGhosts = [
    Ghost {
        ghostType = Blinky,
        ghostPosition = Position (13, 11),
        ghostDirection = Up,
        ghostStatus = Chasing,
        ghostSpeed = Speed 0.2,
        ghostHouseState = OutHouse
    },
    Ghost {
        ghostType = Pinky,
        ghostPosition = Position (13, 14),
        ghostDirection = Up,
        ghostStatus = Chasing,
        ghostSpeed = Speed 0.2,
        ghostHouseState = InHouse
    },
    Ghost {
        ghostType = Inky,
        ghostPosition = Position (12, 14),
        ghostDirection = Up,
        ghostStatus = Chasing,
        ghostSpeed = Speed 0.2,
        ghostHouseState = InHouse
    },
    Ghost {
        ghostType = Clyde,
        ghostPosition = Position (15, 14),
        ghostDirection = Up,
        ghostStatus = Chasing,
        ghostSpeed = Speed 0.2,
        ghostHouseState = InHouse
    }
    ]

initialDots :: [Dot]
initialDots = [Dot (fromInteger (toInteger x), fromInteger (toInteger y)) | x <- [1..29], y <- [1..29], not $ isWall x y]
    where isWall x y = (fromInteger (toInteger x), fromInteger (toInteger y)) `elem` initialWalls

initialPowerUps :: [PowerUp]
initialPowerUps = [
    PowerUp (1.0, 1.0) (Time 10.0) SuperDot True,      -- Bottom left
    PowerUp (28.0, 1.0) (Time 10.0) SuperDot True,     -- Bottom right
    PowerUp (1.0, 28.0) (Time 10.0) SuperDot True,     -- Top left
    PowerUp (28.0, 28.0) (Time 10.0) SuperDot True,    -- Top right
    PowerUp (14.0, 14.0) (Time 5.0) SpeedBoost True    -- Center
    ]

initialWalls :: [(Float, Float)]
initialWalls = 
    -- Outer walls
    [(x, 0) | x <- [0..29]] ++  -- Bottom wall
    [(x, 29) | x <- [0..29]] ++ -- Top wall
    [(0, y) | y <- [0..29]] ++  -- Left wall
    [(29, y) | y <- [0..29]] ++ -- Right wall

    -- Top section
    [(x, 23) | x <- [2..5]] ++ [(x, 23) | x <- [7..11]] ++
    [(x, 23) | x <- [16..20]] ++ [(x, 23) | x <- [22..25]] ++
    
    -- Upper tunnels
    [(2, y) | y <- [20..23]] ++ [(5, y) | y <- [20..23]] ++
    [(22, y) | y <- [20..23]] ++ [(25, y) | y <- [20..23]] ++
    
    -- Middle section barriers
    [(x, 17) | x <- [2..5]] ++ [(x, 17) | x <- [7..11]] ++
    [(x, 17) | x <- [16..20]] ++ [(x, 17) | x <- [22..25]] ++
    
    -- Ghost house
    [(11, 16), (12, 16), (13, 16), (14, 16), (15, 16), (16, 16)] ++  -- Top
    [(11, 13), (12, 13),                     (15, 13), (16, 13)] ++  -- Bottom (gap at 13-14,13)
    [(11, y) | y <- [13..16]] ++                                      -- Left wall
    [(16, y) | y <- [13..16]] ++                                      -- Right wall
    
    -- Lower section barriers
    [(2, y) | y <- [7..13]] ++ [(5, y) | y <- [7..13]] ++
    [(22, y) | y <- [7..13]] ++ [(25, y) | y <- [7..13]] ++
    
    -- Bottom section
    [(x, 7) | x <- [2..5]] ++ [(x, 7) | x <- [7..11]] ++
    [(x, 7) | x <- [16..20]] ++ [(x, 7) | x <- [22..25]] ++
    
    -- T-shaped intersections
    [(13, y) | y <- [7..10]] ++  -- Vertical parts
    [(x, 10) | x <- [11..15]]    -- Horizontal parts

initialPowerPellets :: [PowerPellet]
initialPowerPellets = [
    PowerPellet (Position (1.0, 26.0)) True,    -- Top left
    PowerPellet (Position (28.0, 26.0)) True,   -- Top right
    PowerPellet (Position (1.0, 3.0)) True,     -- Bottom left
    PowerPellet (Position (28.0, 3.0)) True     -- Bottom right
    ]

-- Get starting position for each ghost type
getGhostStartPosition :: GhostType -> Position
getGhostStartPosition Blinky = Position (13, 11)
getGhostStartPosition Pinky  = Position (13, 14)
getGhostStartPosition Inky   = Position (12, 14)
getGhostStartPosition Clyde  = Position (15, 14)
