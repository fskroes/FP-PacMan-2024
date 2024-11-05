module Controller.IO(
    saveGameState,
    loadGameState
) where

import Model.Types(GameState)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS

saveGameState :: GameState -> IO ()
saveGameState gameState = do
    let filePath = "game_state.json"
    Aeson.encodeFile filePath gameState

loadGameState :: IO (Maybe GameState)
loadGameState = do
    let filePath = "game_state.json"
    contents <- BS.readFile filePath
    return $ Aeson.decode contents