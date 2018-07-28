{-# LANGUAGE 
  OverloadedStrings,
  DeriveGeneric,
  RecordWildCards,
  FlexibleInstances,
  DuplicateRecordFields,
  DataKinds,
  GADTs  
#-}

module Main where

import Data.Aeson
import Data.Text (Text)
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad (forever, when, forM_)
import Control.Monad.IO.Class
import GHC.Generics

import qualified Network.WebSockets as WS

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

data NewClientResult = CreatedNewClient | ClientExistsFailure deriving(Show, Generic)
instance ToJSON NewClientResult

data JoinGameResult = JoinedGame | FailedToJoinGame deriving(Show, Generic)
instance ToJSON JoinGameResult

data NewGameResult = CreatedNewGame | FailedToCreateNewGame deriving(Show, Generic)
instance ToJSON NewGameResult

data Direction = Clockwise | CounterClockwise deriving(Show, Generic)
instance FromJSON Direction

newServerState :: ServerState
newServerState = ServerState [] [] []

toMessage :: GameState -> Message
toMessage s = GameStateMsg (running (s::GameState)) $ players (s::GameState)

makePlayer :: Client -> Player
makePlayer Client{..} = Player name connection 0 0

containsName :: [Client] -> Text -> Bool
ps `containsName` n = any (\Client{..} -> name == n) ps

--containsGameName :: [MVar GameSeed] -> Text -> IO Bool
--gs `containsGameName` n = 
--  forM_ gs $ \g -> do
--    game <- readMVar g
--    when (name (game::GameSeed) == n ) $ return True

addClient :: MVar ServerState -> Client -> IO ()
addClient state client = do
  liftIO $ modifyMVar_ state $ \s -> do
    return $ (s :: ServerState) {clients=client:(clients (s::ServerState))}

addGame :: MVar ServerState -> MVar GameSeed -> IO ()
addGame state gameSeed = do
  liftIO $ modifyMVar_ state $ \s -> do
    return $ (s :: ServerState) {gameSeeds=gameSeed:(gameSeeds s)}

listenInInitialContext :: MVar ServerState -> WS.Connection -> IO ()
listenInInitialContext serverState conn = 
  forever $ do
    jsonMsg <- WS.receiveData conn
    case decode jsonMsg :: Maybe Message of
      Just msg@NewClientReqMsg{..} -> handleRequest msg 
      _ -> putStrLn $ "InitialContext received strange message: " ++ show jsonMsg
  where 
    reply = \m ->  WS.sendTextData conn $ encode m
    handleRequest m = do
      state <- readMVar serverState
      let allClients = clients (state :: ServerState)
      let allGames = games state 
      case m of
        NewClientReqMsg{..} ->
          if (allClients `containsName` name)
          then do
            putStrLn $ "Failed to create client: " ++ (show name)
            reply $ NewClientResMsg ClientExistsFailure
          else do 
            let newClient = Client name conn
            addClient serverState newClient
            putStrLn $ "Created client: " ++ (show name)
            reply $ NewClientResMsg CreatedNewClient
            listenInClientContext serverState newClient
        _ -> putStrLn $ "Unknown message received."

listenInClientContext :: MVar ServerState -> Client -> IO ()
listenInClientContext serverState client@Client{..} =
  forever $ do
    jsonMsg <- WS.receiveData connection
    case decode jsonMsg :: Maybe Message of
      Just ListGamesReqMsg -> reply $ NotImplementedMsg
      Just msg@NewGameReqMsg{..} -> handleNewGameReq msg
      Just msg@JoinGameReqMsg{..} -> handleJoinGameReq msg
      _ -> return ()
  where
    reply = \m -> WS.sendTextData connection $ encode m
    handleNewGameReq (NewGameReqMsg gameName) = do
      state <- readMVar serverState
      --if ((games state) `containsGameName` gameName)
      --then do
      --  putStrLn $ "Failed to create game: " ++ (show gameName)
      --  reply $ NewGameResMsg FailedToCreateNewGame
      --else do
      putStrLn $ "Making new game: " ++ show gameName
      newGame gameName serverState
      addPlayer gameName serverState $ makePlayer client
    handleJoinGameReq (JoinGameReqMsg gameName) = do
      state <- readMVar serverState
      putStrLn $ "Client " ++ show name ++ " is joining game " ++ show gameName
      addPlayer gameName serverState $ makePlayer client

--add player to game and start message listener for that player
addPlayer :: Text -> MVar ServerState -> Player -> IO ()
addPlayer gameName state p@Player{..} = do
  serverState <- readMVar state
  let allGames = games serverState
  let maybeGame = first (\(n, _) -> n == gameName) allGames
  case maybeGame of
    Nothing -> return ()
    Just (gameName, game) -> do
      liftIO $ modifyMVar_ game $ \g@GameState{..} -> do
        return $ (g::GameState) {players = p:players}
      listenInGameContext game p
  
first :: (a -> Bool) -> [a] -> Maybe a
first f []     = Nothing
first f (x:xs) = if f x then Just x else first f xs

newGame :: Text -> MVar ServerState -> IO ()
newGame gameName serverState = do
  msgQ <- newChan
  game <- newMVar $ GameState gameName False [] msgQ
  forkIO $ gameMsgHandler msgQ game
  liftIO $ modifyMVar_ serverState $ \s -> do
    return $ (s::ServerState) {games=(gameName, game):(games s)}
  
listenInGameContext :: MVar GameState -> Player -> IO ()
listenInGameContext state Player{..} = do
  gameState <- readMVar state
  let msgQ = queue gameState
  forever $ do 
    jsonMsg <- WS.receiveData connection
    case decode jsonMsg :: Maybe Message of
      Just msg -> writeChan msgQ msg
      Nothing  -> return ()

send :: WS.Connection -> Message -> IO ()
send conn msg = WS.sendTextData conn $ encode msg

broadcastGameState :: MVar GameState -> IO ()
broadcastGameState state = do
  gameState <- readMVar state
  let msg = toMessage gameState
  let ps = players (gameState::GameState)
  forM_ ps $ \p@Player{..} -> send connection msg

broadcast :: Message -> [Client] -> IO ()
broadcast msg clients = forM_ clients $ \Client{..} -> send connection msg

--spawnNewGameEngine :: MVar GameSeed -> IO ThreadId
--spawnNewGameEngine g = do
--  game <- readMVar g
--  msgQ <- newChan
--  let players = map makePlayer $ clients (game::GameSeed)
--  gameState <- newMVar $ GameState False players
--  forkIO $ gameMsgHandler msgQ gameState

gameMsgHandler :: Chan Message -> MVar GameState -> IO ()
gameMsgHandler chan gameState = 
  forever $ do
    msg <- readChan chan
    case msg of
      StartGameReqMsg -> do
        putStrLn $ "Game has been started..."
        liftIO $ modifyMVar_ gameState $ \s -> do
          return $ (s :: GameState) {running=True}
        broadcastGameState gameState
      move@MoveMsg{..} -> do
        putStrLn $ "player " ++ show playerName ++ " moved " ++ show direction
        liftIO $ modifyMVar_ gameState $ \s -> do
          return $ movePlayer s move
        broadcastGameState gameState
      _ -> putStrLn $ "Woah there! whats this message?? " ++ show msg
  where movePlayer s move = 
          (s::GameState){players = map 
                           (\p@Player{..} -> 
                             if name == playerName move 
                             then p{position=(moveDirection position (direction move))}
                             else p) 
                           (players (s::GameState))}
        moveDirection x d = case d of 
          Clockwise -> x-1
          CounterClockwise -> x+1

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do 
  conn <- WS.acceptRequest pending
  putStrLn "Pending connection received"
  WS.forkPingThread conn 30
  listenInInitialContext state conn

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state
