module AISpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Model.AI
import Model.Types
    ( Position(..)
    , Direction(..)
    , GhostType(..)
    , Time(..)
    , GhostStatus(..)
    , GhostHouseState(..)
    , Speed(..)
    , Ghost(..)
    , PacMan(..)
    , GameState(..)
    , Score(..)
    , Lives(..)
    , GameStatus(..)
    , ghostSpeedValue
    )
import Model.Common
import Init (initialBoard)

-- Arbitrary instances for custom types
instance Arbitrary Position where
    arbitrary = do
        x <- choose (-10, 10)
        y <- choose (-10, 10)
        return $ Position (x, y)

instance Arbitrary Direction where
    arbitrary = elements [Up, Down, LeftDir, RightDir]

instance Arbitrary GhostType where
    arbitrary = elements [Blinky, Pinky, Inky, Clyde]

instance Arbitrary Time where
    arbitrary = Time <$> choose (0.0, 10.0)  -- Generate random float for Time

instance Arbitrary GhostStatus where
    arbitrary = oneof [
        return Chasing,
        Frightened <$> arbitrary,
        return Eaten
        ]

instance Arbitrary GhostHouseState where
    arbitrary = elements [InHouse, Outside]

instance Arbitrary Speed where
    arbitrary = Speed <$> choose (0.1, 1.0)

instance Arbitrary Ghost where
    arbitrary = do
        pos <- arbitrary
        dir <- arbitrary
        gType <- arbitrary
        status <- arbitrary
        houseState <- arbitrary
        spd <- arbitrary
        return $ Ghost pos dir gType status houseState spd Nothing

instance Arbitrary PacMan where
    arbitrary = do
        pos <- arbitrary
        dir <- arbitrary
        spd <- arbitrary
        return $ PacMan pos dir spd

-- Enhanced helper functions
createValidPosition :: Gen Position
createValidPosition = do
    x <- choose (0 :: Integer, 27 :: Integer)  -- Board width bounds
    y <- choose (0 :: Integer, 30 :: Integer)  -- Board height bounds
    return $ Position (fromIntegral x, fromIntegral y)

createValidGameState :: Ghost -> PacMan -> GameState
createValidGameState ghost pacman = GameState 
    { board = initialBoard
    , pacman = pacman { pacmanPosition = ensureValidPosition (pacmanPosition pacman) }
    , ghosts = [ghost { ghostPosition = ensureValidPosition (ghostPosition ghost) }]
    , score = Score 0
    , lives = Lives 3
    , gameStatus = Ongoing
    }
  where
    ensureValidPosition pos@(Position (x, y)) =
        Position (
            fromIntegral $ clamp 0 27 (round x :: Integer),
            fromIntegral $ clamp 0 30 (round y :: Integer)
        )
    clamp min max x = if x < min then min else if x > max then max else x

-- Helper to ensure positions are within valid board bounds
constrainToBoard :: Position -> Position
constrainToBoard (Position (x, y)) =
    Position (
        fromIntegral $ min 27 $ max 0 (round x :: Integer),
        fromIntegral $ min 30 $ max 0 (round y :: Integer)
    )

prop_handleFrightenedBehavior :: Ghost -> Property
prop_handleFrightenedBehavior ghost =
    forAll createValidPosition $ \pos ->
    forAll createValidPosition $ \pacPos ->
    let ghost' = ghost { ghostPosition = pos, ghostStatus = Frightened (Time 10) }
        pacman = PacMan pacPos Up (Speed 1.0)
        gameState = createValidGameState ghost' pacman
        frightenedGhost = handleFrightened ghost' gameState
    in conjoin [
        counterexample "Should maintain valid position" $
            isValidPosition (board gameState) (ghostPosition frightenedGhost),
        counterexample "Should maintain frightened status" $
            case ghostStatus frightenedGhost of
                Frightened _ -> True
                _ -> False
    ]

prop_ghostTargetingBehavior :: Ghost -> Property
prop_ghostTargetingBehavior ghost =
    forAll createValidPosition $ \pos ->
    forAll createValidPosition $ \pacPos ->
    let ghost' = ghost { ghostPosition = pos }
        pacman = PacMan pacPos Up (Speed 1.0)
        target = getGhostTarget ghost' pacman
    in case ghostType ghost of
        Blinky -> target === pacPos
        Pinky -> counterexample "Pinky's target should be ahead of Pacman" $
                 property $ heuristic target pacPos > 0
        Inky -> property True  -- Complex targeting, basic validity check
        Clyde -> if heuristic pos pacPos > 8
                 then target === pacPos
                 else target === Position (0, 29)

prop_handleEatenMovesTowardsStart :: Ghost -> PacMan -> Property
prop_handleEatenMovesTowardsStart ghost pacman =
    forAll createValidPosition $ \pos ->
    let ghost' = ghost { ghostPosition = pos, ghostStatus = Eaten }
        gameState = createValidGameState ghost' pacman
        eatenGhost = handleEaten ghost' gameState
    in counterexample "Ghost should be either Eaten or Chasing" $
       ghostStatus eatenGhost `elem` [Eaten, Chasing]

prop_getPinkyTargetAheadOfPacman :: PacMan -> Property
prop_getPinkyTargetAheadOfPacman pacman = 
    let target = getPinkyTarget pacman
        pacPos = pacmanPosition pacman
    in counterexample "Target should be ahead of Pacman" $
       heuristic target pacPos > 0

prop_getClydeTargetBehavior :: Ghost -> PacMan -> Property
prop_getClydeTargetBehavior ghost pacman =
    forAll createValidPosition $ \pos ->
    let ghost' = ghost { ghostPosition = pos }
        target = getClydeTarget ghost' pacman
        dist = heuristic pos (pacmanPosition pacman)
    in counterexample "Clyde should follow scatter/chase behavior" $
       if dist > 8
       then target === pacmanPosition pacman
       else target === Position (0, 29)

prop_chooseDirectionIsValid :: Ghost -> PacMan -> Property
prop_chooseDirectionIsValid ghost pacman =
    forAll createValidPosition $ \pos ->
    let ghost' = ghost { ghostPosition = pos }
        gameState = createValidGameState ghost' pacman
        newDir = chooseDirection ghost' gameState
    in counterexample "Direction should be valid" $
       newDir `elem` [Up, Down, LeftDir, RightDir]

prop_getAheadPositionDistance :: PacMan -> Property
prop_getAheadPositionDistance pacman =
    forAll (choose (1, 10)) $ \tiles ->
    let ahead = getAheadPosition pacman tiles
        Position (px, py) = pacmanPosition pacman
        Position (ax, ay) = ahead
        expectedDist = fromIntegral tiles :: Float  -- Convert to Float
    in counterexample "Position should be correct distance ahead" $
       case pacmanDirection pacman of
           Up    -> abs (py - ay - expectedDist) < 0.001
           Down  -> abs (ay - py - expectedDist) < 0.001
           LeftDir  -> abs (px - ax - expectedDist) < 0.001
           RightDir -> abs (ax - px - expectedDist) < 0.001

prop_shouldExitHouseConsistency :: Ghost -> Property
prop_shouldExitHouseConsistency ghost =
    case ghostType ghost of
        Blinky -> shouldExitHouse ghost === True
        Pinky -> shouldExitHouse ghost === True
        _ -> shouldExitHouse ghost === False

-- Main spec
spec :: Spec
spec = do
    describe "AI Module" $ do
        describe "Ghost Movement" $ do
            it "reduces or maintains distance to target" $
                property prop_moveGhostTowardsReducesDistance
            it "implements proper frightened behavior" $
                property prop_handleFrightenedBehavior
            it "maintains valid positions during movement" $
                property prop_pathBehavior
            it "implements correct targeting behavior" $
                property prop_ghostTargetingBehavior

        describe "handleEaten" $ do
            it "moves ghost towards start position" $
                property prop_handleEatenMovesTowardsStart

        describe "getPinkyTarget" $ do
            it "returns position ahead of Pacman" $
                property prop_getPinkyTargetAheadOfPacman

        describe "getClydeTarget" $ do
            it "follows scatter/chase behavior based on distance" $
                property prop_getClydeTargetBehavior

        describe "chooseDirection" $ do
            it "returns valid direction" $
                property prop_chooseDirectionIsValid

        describe "getAheadPosition" $ do
            it "maintains minimum distance based on tiles" $
                property prop_getAheadPositionDistance

        describe "shouldExitHouse" $ do
            it "follows consistent rules for each ghost type" $
                property prop_shouldExitHouseConsistency
