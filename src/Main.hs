{-# LANGUAGE OverloadedStrings,
  DeriveGeneric,
  RecordWildCards,
  FlexibleInstances #-}

module Main where
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Text (Text, pack)
import Data.UUID
import GHC.Generics

import qualified Network.WebSockets as WS


--The state of the server is represented as two collections of games.
--The left collection is 'forming' games and the right collection is 'formed'
type ServerState = ([Game], [Game])

--A game is a gameId,playerGroup pair
type Game = (Text, [Player])

--A player is a userName,gameName,score,connection
type Player = (Text, Text, Int, WS.Connection)

instance Eq WS.Connection where
  x /= y = True

instance Show WS.Connection where
  show _ = ""
  

--The paddle direction is either clockwise or counter-clockwise
--using the center of the field as the axis of rotation
data PaddleDir = CWise | CCWise deriving (Show, Generic)

instance ToJSON PaddleDir
instance FromJSON PaddleDir

data Message
    = NewGameReqMsg   { ngrUName :: Text, ngrGName :: Text }
    | NewGameResMsg   { ngResult :: GameCreationResult }
    | JoinGameReqMsg  { jgrUName :: Text, jgrGName :: Text }
    | JoinGameResMsg  { jgResult :: JoinGameResult }
    | NewPlayerMsg    { npName :: Text }
    | StartGameReqMsg { sgrGName :: Text }
    | StartGameMsg    { }
    | GameStateMsg    { players :: [Text]
                      , puckVec :: (Double, Double) }
    | CountDownMsg    { tMinus :: Int }
    | MoveMsg         { mUName :: Text, dir :: PaddleDir }
    deriving (Show, Generic)

instance ToJSON Message where
  toJSON NewGameResMsg{..} = object [
    "messageType" .= ("NewGameResMsg" :: Text),
    "result"      .= ngResult ]
  toJSON JoinGameResMsg{..} = object [
    "messageType" .= ("JoinGameResMsg" :: Text),
    "result"      .= jgResult ]
  toJSON NewPlayerMsg{..} = object [
    "messageType" .= ("NewPlayerMsg" :: Text),
    "playerName"  .= npName ]
  toJSON StartGameMsg = object [
    "messageType" .= ("StartGameMsg" :: Text) ]
  toJSON _ = object []

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    kind <- o .: "messageType"
    case kind of
      "NewGameReqMsg"  -> NewGameReqMsg <$> o .: "userName" <*> o .: "gameName"
      "JoinGameReqMsg" -> JoinGameReqMsg <$> o .: "userName" <*> o .: "gameName"
      "StartGameReqMsg" -> StartGameReqMsg <$> o .: "gameName"
      _ -> fail ("unknown message type: " ++ kind)


data GameCreationResult = GameCreated | GameExistsFailure 
                        deriving (Show, Generic)

instance ToJSON GameCreationResult
instance FromJSON GameCreationResult 

data JoinGameResult = JoinedGame | GameDoesNotExist
                    deriving (Show, Generic)

instance ToJSON JoinGameResult
instance FromJSON JoinGameResult


newServerState :: ServerState
newServerState = ([],[])

--gameExists :: Int -> ServerState -> Bool
--gameExists id = any (\g -> gameId g == id)

addPlayer :: Player -> Text -> ServerState -> ServerState
addPlayer player gameName state = ((fmap addPlayerToGame formingGames),formedGames)
  where addPlayerToGame = \(id,players) -> if id == gameName then (id,player:players) else (id, players)
        formingGames = fst state
        formedGames = snd state
  
--newGame creates a 'forming' game with one player 
newGame :: Text -> Player -> ServerState -> ServerState
newGame name player (forming, formed) = (newGame:forming, formed)
  where newGame = (name, [player])

getGame :: Text -> ServerState -> Maybe Game
getGame name (forming, formed) = 
  case length result == 1 of
    True  -> Just $ head result
    False -> Nothing
  where result = filter (\g -> (fst g) == name) (forming ++ formed)

--formGame moves the game from 'forming' to 'formed'
formGame :: Text -> ServerState -> Maybe ServerState
formGame name (forming, formed) = 
  case result of 
    Just game -> Just (filter (\(n,_) -> n /= fst game) forming, game:formed)
    Nothing   -> Nothing
  where result = getGame name (forming, formed)

handleMsg :: MVar ServerState -> Message -> IO ()
handleMsg state StartGameReqMsg{..} = do
  putStrLn "StartGameReqMsg received"
  games <- readMVar state
  liftIO $ modifyMVar_ state $ \s ->
    case formGame sgrGName games of
      Just g -> return g
      Nothing -> return games -- this smells bad. this branch should never be hit. Is this abstracted correctly?
  case getGame sgrGName games of
    Just (_, players) -> broadcast StartGameMsg players
    Nothing -> return () 

logState :: MVar ServerState -> IO ()
logState state = do
  games <- readMVar state
  putStrLn $ show games

-- listen for messages from each player and route them
-- to all other players
listen :: MVar ServerState -> Player -> IO ()
listen state (uName, gName, score, conn) = forever $ do
  jsonMsg <- WS.receiveData conn
  games <- readMVar state
  case decode jsonMsg :: Maybe Message of
    Just msg -> handleMsg state msg
    Nothing -> do 
      putStrLn $ "received unknown msg" 
      putStrLn $ show jsonMsg

broadcast :: ToJSON a => a -> [Player] -> IO ()
broadcast msg players = do
  let m = encode msg
  forM_ players $ \(_, _, _, c) -> WS.sendTextData c m

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  jsonMsg <- WS.receiveData conn
  case decode jsonMsg :: Maybe Message of
    Just req@NewGameReqMsg{..} -> do
      putStrLn "received a NewGameRequestMsg"
      let newPlayer = (ngrUName, ngrGName, 0, conn)
      games <- readMVar state
      case getGame ngrGName games of
        Just _ -> WS.sendTextData conn $ encode $ NewGameResMsg GameExistsFailure
        Nothing -> do
          liftIO $ modifyMVar_ state $ \(forming, formed) -> do
            return $ newGame ngrGName newPlayer games
          WS.sendTextData conn $ encode $ NewGameResMsg GameCreated
          putStrLn "created new game..."
          listen state newPlayer
    Just JoinGameReqMsg{..} -> do
      putStrLn "received a JoinGameRequestMsg"  
      let newPlayer = (jgrUName, jgrGName, 0, conn)
      games <- readMVar state
      case getGame jgrGName games of
        Just _ -> do
          liftIO $ modifyMVar_ state $ \(forming, formed) -> do
            return $ addPlayer newPlayer jgrGName games
          WS.sendTextData conn $ encode $ JoinGameResMsg JoinedGame
          listen state newPlayer 
          -- TODO: keep listening after failures
        Nothing -> do
          putStrLn "did not find game..."
          WS.sendTextData conn $ encode $ JoinGameResMsg GameDoesNotExist
    _ -> do
      putStrLn "received a message which i do not yet know how to handle"
      WS.sendTextData conn ("{\"message\":\"Unknown message received\"}" :: Text)
