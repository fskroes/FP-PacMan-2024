module View.Rendering (
    generateDots,
    convertWallsToMaze,
    renderGameState,
    renderButtons,
    renderPauseText,
    renderPacMan,
    renderGhosts,
    renderDots,
    renderPowerUps,
    renderBoard,
    renderLives,
    renderScore,
    renderStatusMessage,
    renderPowerPellets
) where

import Graphics.Gloss
import Model.Types

-- Helper function to generate dots avoiding walls
generateDots :: [(Float, Float)] -> [Dot]
generateDots walls = 
    [ Dot (x, y) 
    | x <- [1.0..28.0]
    , y <- [1.0..28.0]
    , not (any (\(wx, wy) -> abs (wx - x) < 0.5 && abs (wy - y) < 0.5) walls)
    , not (isInGhostHouse x y)
    ]

isInGhostHouse :: Float -> Float -> Bool
isInGhostHouse x y = 
    x >= 13 && x <= 15 && y >= 8 && y <= 11

-- Helper function to convert wall coordinates to maze
convertWallsToMaze :: [(Float, Float)] -> [[MazeElement]]
convertWallsToMaze walls = 
    [[if isWall x y then Wall else Empty 
      | x <- [0..29]] 
      | y <- [0..29]]
  where
    isWall x y = (fromIntegral x, fromIntegral y) `elem` walls

-- Render the game state
renderGameState :: GameState -> IO Picture
renderGameState gameState = do
    return $ Pictures [
        renderBoard (board gameState),
        renderDots (dots gameState),
        renderPowerUps (powerUps gameState),
        renderPowerPellets (powerPellets gameState),
        renderPacMan (pacman gameState),
        renderGhosts (ghosts gameState),
        renderScore (score gameState),
        renderLives (lives gameState),
        renderButtons,
        renderPauseText (gameStatus gameState),
        renderStatusMessage (statusMessage gameState)
        ]

-- Add button rendering
renderButtons :: Picture
renderButtons = Pictures [
    -- Save button
    translate (180) (-350) $  -- Align with "SCORE" (was -500)
    Pictures [
        color (dark blue) $ rectangleSolid 100 30,
        color blue $ rectangleSolid 96 26,
        translate (-30) (-5) $ 
        scale 0.1 0.1 $ 
        color white $ 
        text "SAVE GAME"
    ],
    
    -- Load button
    translate (180) (-400) $  -- Align with "HIGH SCORE" (was -540)
    Pictures [
        color (dark blue) $ rectangleSolid 100 30,
        color blue $ rectangleSolid 96 26,
        translate (-30) (-5) $ 
        scale 0.1 0.1 $ 
        color white $ 
        text "LOAD GAME"
    ]
    ]

-- Add pause text rendering
renderPauseText :: GameStatus -> Picture
renderPauseText Paused = 
    translate (-50) 0 $ 
    scale 0.3 0.3 $ 
    color yellow $ 
    text "PAUSED"
renderPauseText _ = Blank

renderPacMan :: PacMan -> Picture
renderPacMan pacman = 
    let Position (px, py) = pacmanPosition pacman
        scaledX = px * 20 - 300
        scaledY = py * 20 - 300
    in translate scaledX scaledY $ 
       color (bright yellow) $  
       circleSolid 10

renderGhosts :: [Ghost] -> Picture
renderGhosts ghosts = Pictures [
    let Position (gx, gy) = ghostPosition ghost
        scaledX = gx * 20 - 300
        scaledY = gy * 20 - 300
        ghostColor = case ghostStatus ghost of
            Frightened _ -> bright blue  -- Blue when frightened
            Eaten -> white               -- White when eaten
            Chasing -> case ghostType ghost of  -- Normal colors when chasing
                Blinky -> red
                Pinky -> rose
                Inky -> cyan
                Clyde -> orange
    in translate scaledX scaledY $ 
       color ghostColor $
       circleSolid 10
    | ghost <- ghosts
    ]

renderScore :: Score -> Picture
renderScore (Score value) = 
    Pictures [
        -- Score display
        translate (-280) (-350) $  -- Moved down further (was -320)
        scale 0.15 0.15 $
        color yellow $
        text "SCORE",
        
        translate (-280) (-370) $  -- Moved down (was -340)
        scale 0.15 0.15 $
        color yellow $
        text $ show value,
        
        -- High Score display
        translate (-280) (-400) $  -- Moved down (was -370)
        scale 0.15 0.15 $
        color yellow $
        text "HIGH SCORE",
        
        translate (-280) (-420) $  -- Moved down (was -390)
        scale 0.15 0.15 $
        color yellow $
        text $ show 0
    ]

renderDots :: [Dot] -> Picture
renderDots dots = Pictures [
    let Dot (dx, dy) = dot
        scaledX = dx * 20 - 300
        scaledY = dy * 20 - 300
    in translate scaledX scaledY $ 
       color white $
       circleSolid 2
    | dot <- dots
    ]

renderPowerUps :: [PowerUp] -> Picture
renderPowerUps powerUps = Pictures [
    let (px, py) = powerUpPosition powerUp
        scaledX = px * 20 - 300
        scaledY = py * 20 - 300
    in translate scaledX scaledY $ 
       case powerUpType powerUp of
           SuperDot -> Pictures [  -- Large blue pill
               color (dark blue) $ circleSolid 8,
               color (bright blue) $ circleSolid 6,
               color white $ circleSolid 4
            ]
           SpeedBoost -> Pictures [  -- Red lightning bolt
               color red $ Polygon [
                   (-4, 8), (0, 2), (4, 2),
                   (0, -2), (4, -8), (0, -4),
                   (-4, -4)
               ],
               color (bright red) $ Polygon [
                   (-3, 7), (0, 1), (3, 1),
                   (0, -3), (3, -7), (0, -3),
                   (-3, -3)
               ]
            ]
           PointsMultiplier -> Pictures [  -- Golden star
               color yellow $ Polygon [
                   (0, 8), (2, 2), (8, 2), 
                   (4, -2), (6, -8), (0, -4),
                   (-6, -8), (-4, -2), (-8, 2),
                   (-2, 2)
               ],
               color (bright yellow) $ Polygon [
                   (0, 6), (1.5, 1.5), (6, 1.5),
                   (3, -1.5), (4.5, -6), (0, -3),
                   (-4.5, -6), (-3, -1.5), (-6, 1.5),
                   (-1.5, 1.5)
               ]
            ]
           Invincibility -> Pictures [  -- Shield shape
               color (dark cyan) $ Polygon [
                   (0, 8), (6, 4), (6, -4),
                   (0, -8), (-6, -4), (-6, 4)
               ],
               color cyan $ Polygon [
                   (0, 6), (4, 3), (4, -3),
                   (0, -6), (-4, -3), (-4, 3)
               ],
               color white $ circleSolid 2
            ]
    | powerUp <- powerUps,
      powerUpActive powerUp
    ]
renderPowerPellets :: [PowerPellet] -> Picture
renderPowerPellets pellets = Pictures [
    let Position (x, y) = pelletPosition pellet
        scaledX = x * 20 - 300
        scaledY = y * 20 - 300
    in translate scaledX scaledY $ 
       Pictures [
           color white $ circleSolid 8,        -- Large white center
           color white $ circle 8,             -- White outline
           color white $ circleSolid 6,        -- Inner circle
           color black $ circleSolid 4         -- Black center for contrast
       ]
    | pellet <- pellets,
      pelletActive pellet
    ]

renderBoard :: Board -> Picture
renderBoard gameBoard = Pictures [
    let scaledX = x * 20 - 300
        scaledY = y * 20 - 300
    in translate scaledX scaledY $ 
       color (dark blue) $
       rectangleSolid 20 20
    | (y, row) <- zip [(0::Float)..] (maze gameBoard)
    , (x, cell) <- zip [(0::Float)..] row
    , cell == Wall
    ]

-- Add lives display with Pac-Man icons
renderLives :: Lives -> Picture
renderLives (Lives n) = 
    Pictures [
        -- "LIVES" text
        translate (-280) (-450) $  -- Moved down (was -420)
        scale 0.15 0.15 $
        color yellow $
        text "LIVES",
        
        -- Pac-Man icons for each life
        Pictures [
            translate (-280 + fromIntegral i * 25) (-470) $  -- Moved down (was -440)
            Pictures [
                color yellow $ circleSolid 8,
                color black $ Polygon [
                    (0, 0),
                    (8 * cos angle, 8 * sin angle),
                    (8 * cos (angle + 0.5), 8 * sin (angle + 0.5))
                ]
            ]
            | i <- [0..n-1],
            let angle = pi / 4
        ]
    ]

-- Update status message rendering
renderStatusMessage :: Maybe (String, Float) -> Picture
renderStatusMessage Nothing = Blank
renderStatusMessage (Just (msg, _)) = 
    Pictures [
        -- Black outline/shadow for better visibility
        translate (-49) 51 $
        scale 0.2 0.2 $ 
        color black $ 
        text msg,
        
        -- Main message
        translate (-50) 50 $
        scale 0.2 0.2 $ 
        color yellow $ 
        text msg
    ]