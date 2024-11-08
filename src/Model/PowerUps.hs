module Model.PowerUps(
    updatePowerUps,
    checkPowerUpCollision,
    createPowerUpEffect,
    applyPacmanEffects,
    applyGhostEffects,
    updateEffectTime,
    multiplySpeed,
    effectActive,
    isSuperDotEffect,
    updateScoreWithPowerUp,
    resetGhostStatus
) where

import Model.Types
import Model.Common(oppositeDirection)

-- Update power-up effects
updatePowerUps :: Float -> GameState -> GameState
updatePowerUps deltaTime gameState = gameState {
    activeEffects = updatedEffects,
    ghosts = updatedGhosts,
    pacman = updatedPacman
}
    where
        -- Update remaining time for each effect
        updatedEffects = filter (effectActive . remainingTime) $
            map (updateEffectTime deltaTime) (activeEffects gameState)
        
        -- Update ghosts based on active effects
        updatedGhosts = applyGhostEffects updatedEffects (ghosts gameState)
        
        -- Update pacman based on active effects
        updatedPacman = applyPacmanEffects updatedEffects (pacman gameState)

-- Enhanced power-up collision handling
checkPowerUpCollision :: PacMan -> PowerUp -> GameState -> GameState
checkPowerUpCollision pacman powerUp gameState =
    let Position (px, py) = pacmanPosition pacman
        (pux, puy) = powerUpPosition powerUp
        -- Check if Pac-Man is close enough to collect the power-up
        isCollected = abs (px - pux) < 0.5 && abs (py - puy) < 0.5 && powerUpActive powerUp
    in if isCollected
       then gameState {
           powerUps = filter (/= powerUp) (powerUps gameState),
           activeEffects = createPowerUpEffect (powerUpType powerUp) : activeEffects gameState,
           score = updateScoreWithPowerUp (score gameState) (powerUpType powerUp)
       }
       else gameState

-- Create a power-up effect when collected
createPowerUpEffect :: PowerUpType -> PowerUpEffect
createPowerUpEffect powerUpType = 
    case powerUpType of
        SuperDot -> PowerUpEffect {
            effectType = SuperDot,
            remainingTime = Time 10.0,  -- 10 seconds of ghost frightened mode
            effectStrength = 1.0
        }
        SpeedBoost -> PowerUpEffect {
            effectType = SpeedBoost,
            remainingTime = Time 5.0,   -- 5 seconds of speed boost
            effectStrength = 1.5        -- 50% speed increase
        }
        PointsMultiplier -> PowerUpEffect {
            effectType = PointsMultiplier,
            remainingTime = Time 15.0,  -- 15 seconds of double points
            effectStrength = 2.0        -- Double points
        }
        Invincibility -> PowerUpEffect {
            effectType = Invincibility,
            remainingTime = Time 8.0,   -- 8 seconds of invincibility
            effectStrength = 1.0
        }

-- Apply effects to Pacman
applyPacmanEffects :: [PowerUpEffect] -> PacMan -> PacMan
applyPacmanEffects effects pacman =
    foldr applyEffect pacman effects
    where
        applyEffect effect pac = case effectType effect of
            SpeedBoost -> pac { 
                pacmanSpeed = multiplySpeed (pacmanSpeed pac) (effectStrength effect) 
            }
            Invincibility -> pac  -- Invincibility handled in collision detection
            _ -> pac

-- Apply effects to ghosts
applyGhostEffects :: [PowerUpEffect] -> [Ghost] -> [Ghost]
applyGhostEffects effects ghosts = 
    let superDotActive = any (isSuperDotEffect . effectType) effects
    in if superDotActive
        then map (\ghost -> ghost { 
            ghostStatus = Frightened (Time 10.0),
            ghostSpeed = Speed (ghostSpeedValue baseGhostSpeed * frightenedSpeedMultiplier),
            ghostDirection = oppositeDirection (ghostDirection ghost) 
        }) ghosts
        else map resetGhostStatus ghosts

-- Update effect time
updateEffectTime :: Float -> PowerUpEffect -> PowerUpEffect
updateEffectTime deltaTime effect = effect {
    remainingTime = Time (currentTime - deltaTime)
}
    where Time currentTime = remainingTime effect

-- Helper to multiply speed
multiplySpeed :: Speed -> Float -> Speed
multiplySpeed (Speed s) multiplier = Speed (s * multiplier)

-- Check if effect is still active
effectActive :: Time -> Bool
effectActive (Time t) = t > 0

-- Helper to check if an effect is a SuperDot effect
isSuperDotEffect :: PowerUpType -> Bool
isSuperDotEffect SuperDot = True
isSuperDotEffect _ = False

-- Update score based on power-up type
updateScoreWithPowerUp :: Score -> PowerUpType -> Score
updateScoreWithPowerUp (Score currentScore) powerUpType =
    Score $ currentScore + case powerUpType of
        SuperDot -> 50
        SpeedBoost -> 30
        PointsMultiplier -> 100
        Invincibility -> 75

-- Reset ghost status when effects wear off
resetGhostStatus :: Ghost -> Ghost
resetGhostStatus ghost = case ghostStatus ghost of
    Frightened _ -> ghost { 
        ghostStatus = Chasing,
        ghostSpeed = baseGhostSpeed,
        pathCache = Nothing  -- Reset path when status changes
    }
    _ -> ghost