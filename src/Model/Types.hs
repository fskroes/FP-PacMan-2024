{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Types(
    Score(..),
    Lives(..),
    GameStatus(..),
    GhostHouseState(..),
    GameState(..),
    Speed(..),
    Position(..),
    Time(..),
    PacMan(..),
    GhostType(..),
    GhostStatus(..),
    Direction(..),
    Ghost(..),
    Cell(..),
    MazeElement(..),
    Board(..),
    Dot(..),
    PowerUp(..),
    PowerPellet(..),
    PowerUpType(..),
    PowerUpEffect(..),
    Node(..),
    ghostSpeedValue
) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Text as T
import Data.Ord (comparing)

newtype Score = Score Int
    deriving (Show)
newtype Lives = Lives Int
    deriving (Show)
data GameStatus = Ongoing | Won | Loss | Paused 
    deriving (Show)
data GhostHouseState = InHouse | OutHouse | Waiting Time | Exiting | Outside
    deriving (Show, Eq)

data GameState = GameState {
    board           :: Board,
    pacman          :: PacMan,
    ghosts          :: [Ghost],
    dots            :: [Dot],
    powerUps        :: [PowerUp],
    activeEffects   :: [PowerUpEffect],
    score           :: Score,
    lives           :: Lives,
    gameStatus      :: GameStatus,
    powerPellets    :: [PowerPellet],
    statusMessage   :: Maybe (String, Float)  -- (Message, Time remaining to show)
} deriving (Show)

newtype Speed       = Speed Float
    deriving (Show, Eq)
newtype Position = Position (Float, Float)
    deriving (Show, Eq)

newtype Time        = Time Float
instance Show Time where
    show :: Time -> String
    show (Time t) = show t
instance Eq Time where
    (==) :: Time -> Time -> Bool
    (Time t1) == (Time t2) = t1 == t2

data PacMan = PacMan {
    pacmanPosition  :: Position,
    pacmanDirection :: Direction,
    pacmanSpeed     :: Speed
} deriving (Show)

data GhostType      = Blinky | Pinky | Inky | Clyde 
    deriving (Show, Eq)
data GhostStatus = Chasing | Frightened Time | Eaten
    deriving (Show, Eq)

data Direction      = Up | Down | LeftDir | RightDir
    deriving (Eq, Show)

data Ghost = Ghost {
    ghostType       :: GhostType,
    ghostPosition   :: Position,
    ghostDirection  :: Direction,
    ghostStatus     :: GhostStatus,
    ghostSpeed      :: Speed,
    ghostHouseState :: GhostHouseState
} deriving (Show, Eq)

data Cell = WallCell | EmptyCell | DotCell | PowerUpCell
    deriving (Show)

data MazeElement = Wall | Empty | DotTile | PowerPelletTile
    deriving (Show, Eq)

data Board = Board {
    maze :: [[MazeElement]],
    width :: Int,
    height :: Int
} deriving (Show)

newtype Dot = Dot {
    dotPosition :: (Float, Float)
} deriving (Show)

data PowerUp = PowerUp {
    powerUpPosition :: (Float, Float),
    powerUpDuration :: Time,
    powerUpType     :: PowerUpType,
    powerUpActive   :: Bool  -- Track if power-up is currently active
} deriving (Show)

instance Eq PowerUp where
    p1 == p2 = powerUpPosition p1 == powerUpPosition p2 
             && powerUpType p1 == powerUpType p2
             && powerUpActive p1 == powerUpActive p2

data PowerPellet = PowerPellet {
    pelletPosition :: Position,
    pelletActive :: Bool
} deriving (Show)

data PowerUpType = 
    SuperDot       -- Makes ghosts frightened
    | SpeedBoost   -- Increases Pacman's speed
    | PointsMultiplier  -- Doubles points for a duration
    | Invincibility     -- Pacman can't be killed
    deriving (Show, Eq)

data PowerUpEffect = PowerUpEffect {
    effectType     :: PowerUpType,
    remainingTime  :: Time,
    effectStrength :: Float  -- For variable strength effects
} deriving (Show)

-- A* Node type
data Node = Node {
    nodePosition :: Position,
    gScore :: Float,  -- Cost from start
    hScore :: Float,  -- Estimated cost to goal
    parent :: Maybe Node
} deriving (Show, Eq)

instance Ord Node where
    compare = comparing (\n -> gScore n + hScore n)

ghostSpeedValue :: Speed -> Float
ghostSpeedValue (Speed s) = s

-- Add these instances for GameState
instance Aeson.ToJSON GameState where
    toJSON (GameState board pacman ghosts dots powerups effects score lives status pellets msg) =
        Aeson.object [ Key.fromString "board" .= board
                    , Key.fromString "pacman" .= pacman
                    , Key.fromString "ghosts" .= ghosts
                    , Key.fromString "dots" .= dots
                    , Key.fromString "powerups" .= powerups
                    , Key.fromString "activeEffects" .= effects
                    , Key.fromString "score" .= score
                    , Key.fromString "lives" .= lives
                    , Key.fromString "gameStatus" .= status
                    , Key.fromString "powerPellets" .= pellets
                    , Key.fromString "statusMessage" .= msg
                    ]

instance Aeson.FromJSON GameState where
    parseJSON = Aeson.withObject "GameState" $ \v -> 
        GameState <$> v .: Key.fromString "board"
                 <*> v .: Key.fromString "pacman"
                 <*> v .: Key.fromString "ghosts"
                 <*> v .: Key.fromString "dots"
                 <*> v .: Key.fromString "powerups"
                 <*> v .: Key.fromString "activeEffects"
                 <*> v .: Key.fromString "score"
                 <*> v .: Key.fromString "lives"
                 <*> v .: Key.fromString "gameStatus"
                 <*> v .: Key.fromString "powerPellets"
                 <*> v .: Key.fromString "statusMessage"

-- Add ToJSON/FromJSON instances for basic types
instance Aeson.ToJSON Score where
    toJSON (Score n) = Aeson.toJSON n

instance Aeson.FromJSON Score where
    parseJSON v = Score <$> Aeson.parseJSON v

instance Aeson.ToJSON Lives where
    toJSON (Lives n) = Aeson.toJSON n

instance Aeson.FromJSON Lives where
    parseJSON v = Lives <$> Aeson.parseJSON v

instance Aeson.ToJSON GameStatus where
    toJSON = Aeson.String . \case
        Ongoing -> T.pack "ongoing"
        Won -> T.pack "won"
        Loss -> T.pack "loss"
        Paused -> T.pack "paused"

instance Aeson.FromJSON GameStatus where
    parseJSON = Aeson.withText "GameStatus" $ \case
        "ongoing" -> pure Ongoing
        "won" -> pure Won
        "loss" -> pure Loss
        "paused" -> pure Paused
        _ -> fail "Invalid GameStatus"

instance Aeson.ToJSON Position where
    toJSON (Position (x, y)) = Aeson.object [ Key.fromString "x" .= x, Key.fromString "y" .= y ]

instance Aeson.FromJSON Position where
    parseJSON = Aeson.withObject "Position" $ \v ->
        Position <$> ((,) <$> v .: Key.fromString "x" <*> v .: Key.fromString "y")

instance Aeson.ToJSON Direction where
    toJSON = Aeson.String . \case
        Up -> T.pack "up"
        Down -> T.pack "down"
        LeftDir -> T.pack "left"
        RightDir -> T.pack "right"

instance Aeson.FromJSON Direction where
    parseJSON = Aeson.withText "Direction" $ \case
        "up" -> pure Up
        "down" -> pure Down
        "left" -> pure LeftDir
        "right" -> pure RightDir
        _ -> fail "Invalid Direction"

instance Aeson.ToJSON Speed where
    toJSON (Speed n) = Aeson.toJSON n

instance Aeson.FromJSON Speed where
    parseJSON v = Speed <$> Aeson.parseJSON v

instance Aeson.ToJSON Board where
    toJSON (Board walls w h) = Aeson.object [
        "walls" .= walls,
        "width" .= w,
        Key.fromString "height" .= h 
        ]

instance Aeson.FromJSON Board where
    parseJSON = Aeson.withObject "Board" $ \v ->
        Board <$> v .: Key.fromString "walls"
              <*> v .: Key.fromString "width"
              <*> v .: Key.fromString "height"

instance Aeson.ToJSON PacMan where
    toJSON (PacMan pos dir spd) = Aeson.object [
        "position" .= pos,
        "direction" .= dir,
        "speed" .= spd 
        ]

instance Aeson.FromJSON PacMan where
    parseJSON = Aeson.withObject "PacMan" $ \v ->
        PacMan <$> v .: Key.fromString "position"
               <*> v .: Key.fromString "direction"
               <*> v .: Key.fromString "speed"

instance Aeson.ToJSON GhostType where
    toJSON = Aeson.String . \case
        Blinky -> T.pack "blinky"
        Pinky -> T.pack "pinky"
        Inky -> T.pack "inky"
        Clyde -> T.pack "clyde"

instance Aeson.FromJSON GhostType where
    parseJSON = Aeson.withText "GhostType" $ \case
        "blinky" -> pure Blinky
        "pinky" -> pure Pinky
        "inky" -> pure Inky
        "clyde" -> pure Clyde
        _ -> fail "Invalid GhostType"

instance Aeson.ToJSON GhostStatus where
    toJSON = \case
        Chasing -> Aeson.object [ "type" .= ("chasing" :: String) ]
        Frightened t -> Aeson.object [ "type" .= ("frightened" :: String), "time" .= t ]
        Eaten -> Aeson.object [ "type" .= ("eaten" :: String) ]

instance Aeson.FromJSON GhostStatus where
    parseJSON = Aeson.withObject "GhostStatus" $ \v -> do
        typ <- v .: "type" :: Aeson.Parser String
        case typ of
            "chasing" -> pure Chasing
            "frightened" -> Frightened <$> v .: "time"
            "eaten" -> pure Eaten
            _ -> fail "Invalid GhostStatus"

instance Aeson.ToJSON Ghost where
    toJSON (Ghost typ pos dir status spd house) = Aeson.object [
        "type" .= typ,
        "position" .= pos,
        "direction" .= dir,
        "status" .= status,
        "speed" .= spd,
        "houseState" .= house
        ]

instance Aeson.FromJSON Ghost where
    parseJSON = Aeson.withObject "Ghost" $ \v ->
        Ghost <$> v .: Key.fromString "type"
              <*> v .: Key.fromString "position"
              <*> v .: Key.fromString "direction"
              <*> v .: Key.fromString "status"
              <*> v .: Key.fromString "speed"
              <*> v .: Key.fromString "houseState"

instance Aeson.ToJSON Dot where
    toJSON (Dot pos) = Aeson.object [ "position" .= pos ]

instance Aeson.FromJSON Dot where
    parseJSON = Aeson.withObject "Dot" $ \v ->
        Dot <$> v .: Key.fromString "position"

instance Aeson.ToJSON PowerUpType where
    toJSON = Aeson.String . \case
        SuperDot -> T.pack "superdot"
        SpeedBoost -> T.pack "speedboost"
        PointsMultiplier -> T.pack "pointsmultiplier"
        Invincibility -> T.pack "invincibility"

instance Aeson.FromJSON PowerUpType where
    parseJSON = Aeson.withText "PowerUpType" $ \case
        "superdot" -> pure SuperDot
        "speedboost" -> pure SpeedBoost
        "pointsmultiplier" -> pure PointsMultiplier
        "invincibility" -> pure Invincibility
        _ -> fail "Invalid PowerUpType"

instance Aeson.ToJSON PowerUp where
    toJSON (PowerUp pos dur typ active) = Aeson.object [
        Key.fromString "position" .= pos,
        Key.fromString "duration" .= dur,
        Key.fromString "type" .= typ,
        Key.fromString "active" .= active
        ]

instance Aeson.FromJSON PowerUp where
    parseJSON = Aeson.withObject "PowerUp" $ \v ->
        PowerUp <$> v .: Key.fromString "position"
                <*> v .: Key.fromString "duration"
                <*> v .: Key.fromString "type"
                <*> v .: Key.fromString "active"

instance Aeson.ToJSON PowerUpEffect where
    toJSON (PowerUpEffect typ time strength) = Aeson.object [
        Key.fromString "type" .= typ,
        Key.fromString "remainingTime" .= time,
        Key.fromString "strength" .= strength
        ]

instance Aeson.FromJSON PowerUpEffect where
    parseJSON = Aeson.withObject "PowerUpEffect" $ \v ->
        PowerUpEffect <$> v .: Key.fromString "type"
                     <*> v .: Key.fromString "remainingTime"
                     <*> v .: Key.fromString "strength"

instance Aeson.ToJSON PowerPellet where
    toJSON (PowerPellet pos active) = Aeson.object [
        Key.fromString "position" .= pos,
        Key.fromString "active" .= active
        ]

instance Aeson.FromJSON PowerPellet where
    parseJSON = Aeson.withObject "PowerPellet" $ \v ->
        PowerPellet <$> v .: Key.fromString "position"
                   <*> v .: Key.fromString "active"

instance Aeson.ToJSON MazeElement where
    toJSON = Aeson.String . \case
        Wall -> T.pack "wall"
        Empty -> T.pack "empty"
        DotTile -> T.pack "dot"
        PowerPelletTile -> T.pack "powerpellet"

instance Aeson.FromJSON MazeElement where
    parseJSON = Aeson.withText "MazeElement" $ \case
        "wall" -> pure Wall
        "empty" -> pure Empty
        "dot" -> pure DotTile
        "powerpellet" -> pure PowerPelletTile
        _ -> fail "Invalid MazeElement"

instance Aeson.ToJSON GhostHouseState where
    toJSON = \case
        InHouse -> Aeson.object [ "type" .= ("inhouse" :: String) ]
        OutHouse -> Aeson.object [ "type" .= ("outhouse" :: String) ]
        Waiting t -> Aeson.object [ "type" .= ("waiting" :: String), "time" .= t ]
        Exiting -> Aeson.object [ "type" .= ("exiting" :: String) ]
        Outside -> Aeson.object [ "type" .= ("outside" :: String) ]

instance Aeson.FromJSON GhostHouseState where
    parseJSON = Aeson.withObject "GhostHouseState" $ \v -> do
        typ <- v .: "type" :: Aeson.Parser String
        case typ of
            "inhouse" -> pure InHouse
            "outhouse" -> pure OutHouse
            "waiting" -> Waiting <$> v .: "time"
            "exiting" -> pure Exiting
            "outside" -> pure Outside
            _ -> fail "Invalid GhostHouseState"

instance Aeson.ToJSON Time where
    toJSON (Time t) = Aeson.Number (realToFrac t)

instance Aeson.FromJSON Time where
    parseJSON v = Time <$> (realToFrac <$> (Aeson.parseJSON v :: Aeson.Parser Double))

instance Ord Position where
    compare (Position (x1, y1)) (Position (x2, y2)) = 
        case compare x1 x2 of
            EQ -> compare y1 y2
            other -> other