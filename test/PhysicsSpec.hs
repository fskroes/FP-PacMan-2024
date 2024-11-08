module PhysicsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property ((.&&.))
import Model.Physics
import Model.Types
import Model.Common

-- Arbitrary instances for our custom types
-- Update Position Arbitrary instance to generate only valid board coordinates
-- Update Position Arbitrary instance to use explicit Integer type
instance Arbitrary Position where
    arbitrary = do
        x <- choose (0 :: Integer, 9 :: Integer)  -- Match board width-1
        y <- choose (0 :: Integer, 9 :: Integer)  -- Match board height-1
        return $ Position (fromIntegral x, fromIntegral y)

instance Arbitrary Direction where
    arbitrary = elements [Up, Down, LeftDir, RightDir]

instance Arbitrary GhostStatus where
    arbitrary = do
        t <- choose (0, 10)
        elements [Chasing, Frightened (Time t), Eaten]

instance Arbitrary Speed where
    arbitrary = Speed <$> choose (0.1, 1.0)

instance Arbitrary GhostHouseState where
    arbitrary = elements [InHouse, Outside]

instance Arbitrary GhostType where
    arbitrary = elements [Blinky, Pinky, Inky, Clyde]

instance Arbitrary Ghost where
    arbitrary = do
        pos <- arbitrary
        dir <- arbitrary
        gType <- arbitrary
        status <- arbitrary
        houseState <- arbitrary
        spd <- arbitrary
        return $ Ghost pos dir gType status houseState spd Nothing

-- Update PacMan Arbitrary instance to ensure valid initial positions
instance Arbitrary PacMan where
    arbitrary = do
        pos <- arbitrary  -- Now uses constrained Position generator
        dir <- arbitrary
        return $ PacMan pos dir (Speed 1.0)

instance Arbitrary GameStatus where
    arbitrary = elements [Ongoing, Won, Loss, Paused]

-- Create a simple test board and make it the standard test board
testBoard :: Board
testBoard = Board {
    width = 10,
    height = 10,
    maze = replicate 10 (replicate 10 Empty)
}

-- Properties
prop_isColliding_reflexive :: Position -> Property
prop_isColliding_reflexive pos =
    isColliding pos pos === True

prop_isColliding_symmetric :: Position -> Position -> Property
prop_isColliding_symmetric pos1 pos2 =
    isColliding pos1 pos2 === isColliding pos2 pos1

-- Update prop_movePacMan_preservesBoard to validate initial position
prop_movePacMan_preservesBoard :: PacMan -> Direction -> Property
prop_movePacMan_preservesBoard pacman dir =
    isValidPosition testBoard (pacmanPosition pacman) ==>  -- Pre-condition
    let newPacman = movePacMan pacman dir testBoard
    in isValidPosition testBoard (pacmanPosition newPacman)

prop_moveGhost_preservesStatus :: Ghost -> Property
prop_moveGhost_preservesStatus ghost =
    let gameState = GameState 
            { board = testBoard
            , ghosts = [ghost]
            , pacman = PacMan (Position (0,0)) RightDir (Speed 1.0) 
            , score = Score 0
            , gameStatus = Ongoing
            , powerUps = []
            , lives = Lives 3
            , powerPellets = []
            , dots = []                -- Add missing fields
            , activeEffects = []       -- Add missing fields
            , statusMessage = Nothing  -- Add missing fields
            , dotsEaten = 0           -- Add the missing dotsEaten field
            }
        newGhost = moveGhost ghost gameState
    in case (ghostStatus ghost, ghostStatus newGhost) of
        (Frightened _, Frightened _) -> property True
        (x, y) -> x === y

-- New test cases for PacMan movement
prop_pacmanGridSnapping :: PacMan -> Direction -> Property
prop_pacmanGridSnapping pacman dir =
    let moved = movePacMan pacman dir testBoard
        Position (x, y) = pacmanPosition moved
        isSnapped = abs (x - fromIntegral (round x)) < 0.1 &&
                    abs (y - fromIntegral (round y)) < 0.1
    in if isValidPosition testBoard (pacmanPosition moved)
       then property isSnapped
       else property $ pacmanPosition moved == pacmanPosition pacman

prop_pacmanDirectionChange :: PacMan -> Direction -> Property
prop_pacmanDirectionChange pacman newDir =
    let Position (x, y) = pacmanPosition pacman
        isOnGrid = abs (x - fromIntegral (round x)) < 0.1 &&
                   abs (y - fromIntegral (round y)) < 0.1
        moved = movePacMan pacman newDir testBoard
    in if isOnGrid && isValidPosition testBoard (pacmanPosition moved)
       then pacmanDirection moved === newDir
       else pacmanDirection moved === pacmanDirection pacman

-- New tests for ghost house transitions
prop_ghostHouseExiting :: Property
prop_ghostHouseExiting =
    forAll (arbitrary `suchThat` (\g -> ghostHouseState g == Exiting)) $ \ghost ->
    let moved = handleExiting ghost
        exitPos = Position (14.0, 11.0)
    in (if ghostPosition moved == exitPos 
        then property $ ghostHouseState moved == Outside
        else property True)
    .&&.
    property (ghostDirection moved == Up)

prop_ghostInHouseMovement :: Ghost -> Property
prop_ghostInHouseMovement ghost =
    ghostHouseState ghost == InHouse ==>
    let moved = handleInHouse ghost
        Position (_, y1) = ghostPosition ghost
        Position (_, y2) = ghostPosition moved
    in property $ y2 <= y1

-- New tests for collision detection
prop_collidingSymmetric :: Position -> Position -> Property
prop_collidingSymmetric p1 p2 =
    property $ isColliding p1 p2 == isColliding p2 p1

prop_collidingThreshold :: Position -> Property
prop_collidingThreshold p1 =
    let Position (x, y) = p1
        p2 = Position (x + 0.3, y + 0.3)  -- Reduced from 0.4 to 0.3 to be within threshold
    in property $ isColliding p1 p2
    -- Note: isColliding uses threshold = 0.5, so total distance should be < 0.5
    -- With x+0.3 and y+0.3, total distance = sqrt(0.3^2 + 0.3^2) ≈ 0.424 < 0.5

spec :: Spec
spec = do
    describe "Physics" $ do
        describe "isColliding" $ do
            it "is reflexive" $
                property prop_isColliding_reflexive
            it "is symmetric" $
                property prop_isColliding_symmetric
        
        describe "movePacMan" $ do
            it "keeps PacMan within valid board positions" $
                property prop_movePacMan_preservesBoard

        describe "moveGhost" $ do
            it "preserves ghost status type (except timer updates)" $
                property prop_moveGhost_preservesStatus
        
        describe "PacMan Movement" $ do
            it "maintains grid snapping" $
                withMaxSuccess 100 prop_pacmanGridSnapping
            it "changes direction only when on grid" $
                withMaxSuccess 100 prop_pacmanDirectionChange

        describe "Ghost House" $ do
            it "handles exiting transition correctly" $
                withMaxSuccess 100 prop_ghostHouseExiting
            it "maintains upward movement in house" $
                withMaxSuccess 100 prop_ghostInHouseMovement

        describe "Collision Detection" $ do
            it "is symmetric" $
                withMaxSuccess 100 prop_collidingSymmetric
            it "detects collisions within threshold" $
                withMaxSuccess 100 prop_collidingThreshold
