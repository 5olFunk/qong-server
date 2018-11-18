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
  , puckPos :: Vec3
  , puckVel :: Vec3
  , queue :: TQueue Message
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
                      , players :: [Player] 
                      , puckPos :: Vec3
                      , puckVel :: Vec3
                      }
  | MoveMsg           { playerName :: Text
                      , direction :: Direction }
  | NotImplementedMsg { }
  deriving (Show, Generic)

data Vec3
  = Vec3
  { x :: Float
  , y :: Float 
  , z :: Float
  } deriving (Show, Generic)

addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 (Vec3 ax ay az) (Vec3 bx by bz) =
  Vec3 (ax + bx) (ay + by) (az + bz)

toMessage :: GameState -> Message
toMessage (GameState _ running players puckPos puckVel _ _) = 
  GameStateMsg running players puckPos puckVel

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
    "players"     .= players,
    "puckPos"     .= object [
                       "x" .= x puckPos,
                       "y" .= y puckPos,
                       "z" .= z puckPos
                     ],
    "puckVel"     .= object [
                       "x" .= x puckVel,
                       "y" .= y puckVel,
                       "z" .= z puckVel
                     ]
     ]
  toJSON msg = object [ "msg" .= ("Error, can't serialize object" :: Text) ]

data NewClientResult = CreatedNewClient | ClientExistsFailure deriving(Show, Generic)
instance ToJSON NewClientResult

data JoinGameResult = JoinedGame | FailedToJoinGame deriving(Show, Generic)
instance ToJSON JoinGameResult

data NewGameResult = CreatedNewGame | FailedToCreateNewGame deriving(Show, Generic)
instance ToJSON NewGameResult

data Direction = Clockwise | CounterClockwise deriving(Show, Generic)
instance FromJSON Direction

