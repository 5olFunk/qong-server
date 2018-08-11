{-# LANGUAGE 
  OverloadedStrings,
  DeriveGeneric,
  RecordWildCards,
  FlexibleInstances,
  DuplicateRecordFields,
  DataKinds,
  GADTs  
#-}

module Models where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import qualified Network.WebSockets as WS
import GHC.Generics


data ServerState = ServerState 
  { clients :: [Client] 
  , gameSeeds :: [MVar GameSeed]
  , games :: [(Text, MVar GameState)]
  }

data GameSeed = GameSeed
  { name :: Text
  , clients :: [Client] 
  }

data GameState = GameState
  { name :: Text
  , running :: Bool
  , players :: [Player]
  , queue :: Chan Message
  , time :: NominalDiffTime
  }

data Client = Client
  { name :: Text
  , connection :: WS.Connection
  }

data Player = Player
  { name :: Text
  , connection :: WS.Connection
  , score :: Int
  , position :: Int
  }

instance Show Player where
  show Player{..} = show name ++ "," ++ show score ++ "," ++ show position

instance ToJSON Player where
  toJSON Player{..} = object [
    "name" .= name,
    "score" .= score,
    "position" .= position ]

data Message 
  = NewClientReqMsg   { name :: Text }
  | NewClientResMsg   { newClientResult :: NewClientResult }
  | NewPlayerMsg      { gameName :: Text
                      , player :: Player }
  | ListGamesReqMsg   { }
  | ListGamesResMsg   { gameNames :: [Text] }
  | NewGameReqMsg     { gameName :: Text }
  | NewGameResMsg     { newGameResult :: NewGameResult }
  | JoinGameReqMsg    { gameName :: Text }
  | JoinGameResMsg    { joinGameResult :: JoinGameResult }
  | StartGameReqMsg   { }
  | GameStateMsg      { running :: Bool
                      , players :: [Player] }
  | MoveMsg           { playerName :: Text
                      , direction :: Direction }
  | NotImplementedMsg { }
  deriving (Show, Generic)

toMessage :: GameState -> Message
toMessage s = GameStateMsg (running (s::GameState)) $ players (s::GameState)

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do 
    kind <- o .: "messageType"
    case kind of
      "NewClientReqMsg" -> NewClientReqMsg <$> o .: "name"
      "ListGamesReqMsg" -> return (ListGamesReqMsg)
      "JoinGameReqMsg"  -> JoinGameReqMsg <$> o .: "gameName"
      "NewGameReqMsg"   -> NewGameReqMsg <$> o .: "gameName"
      "StartGameReqMsg" -> return (StartGameReqMsg)
      "MoveMsg"         -> MoveMsg <$> o .: "name" <*> o .: "direction"
      _ -> fail ("unknown message type: " ++ kind)

instance ToJSON Message where
  toJSON NewClientResMsg{..} = object [
    "messageType" .= ("NewClientResMsg" :: Text),
    "result"      .= newClientResult ]
  toJSON NewGameResMsg{..} = object [
    "messageType" .= ("NewGameResMsg" :: Text),
    "result"      .= newGameResult ]
  toJSON NewPlayerMsg{..} = object [
    "messageType" .= ("NewPlayerMsg" :: Text),
    "gameName"    .= gameName,
    "player"      .= player]
  toJSON ListGamesResMsg{..} = object [
    "messageType" .= ("ListGamesResMsg" :: Text),
    "gameNames"   .= gameNames ]
  toJSON NotImplementedMsg = object [
    "messageType" .= ("NotImplementedMsg" :: Text) ]
  toJSON JoinGameResMsg{..} = object [
    "messageType" .= ("JoinGameResMsg" :: Text),
    "result"      .= joinGameResult ]
  toJSON GameStateMsg{..} = object [
    "messageType" .= ("GameStateMsg" :: Text),
    "running"     .= running,
    "players"     .= players ]
  toJSON msg = object [ "msg" .= ("Error, can't serialize object" :: Text) ]

data NewClientResult = CreatedNewClient | ClientExistsFailure deriving(Show, Generic)
instance ToJSON NewClientResult

data JoinGameResult = JoinedGame | FailedToJoinGame deriving(Show, Generic)
instance ToJSON JoinGameResult

data NewGameResult = CreatedNewGame | FailedToCreateNewGame deriving(Show, Generic)
instance ToJSON NewGameResult

data Direction = Clockwise | CounterClockwise deriving(Show, Generic)
instance FromJSON Direction

