module PhysicsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
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

-- Create a simple test board
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
            , pacman = PacMan (Position (0,0)) RightDir (Speed 1.0)  -- Use Speed constructor
            , score = Score 0
            , gameStatus = Ongoing
            , powerUps = []
            , lives = Lives 3
            , powerPellets = []
            }
        newGhost = moveGhost ghost gameState
    in case (ghostStatus ghost, ghostStatus newGhost) of
        (Frightened _, Frightened _) -> property True
        (x, y) -> x === y

-- Test spec
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
