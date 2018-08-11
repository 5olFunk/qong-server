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
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad (forever, when, forM_)
import Control.Monad.IO.Class
import GHC.Generics
import Models
import Game
import qualified Network.WebSockets as WS


newServerState :: ServerState
newServerState = ServerState [] [] []

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
      reply $ NewGameResMsg CreatedNewGame
      --putStrLn $ "Making new game: " ++ show gameName
      newGame gameName serverState
      addPlayer gameName serverState $ makePlayer client
      
    handleJoinGameReq (JoinGameReqMsg gameName) = do
      state <- readMVar serverState
      reply $ JoinGameResMsg JoinedGame
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
      g <- readMVar game
      let msgQ = queue g
      writeChan msgQ $ NewPlayerMsg gameName p
      listenInGameContext game p

  
first :: (a -> Bool) -> [a] -> Maybe a
first f []     = Nothing
first f (x:xs) = if f x then Just x else first f xs

newGame :: Text -> MVar ServerState -> IO ()
newGame gameName serverState = do
  msgQ <- newChan
  time <- getPOSIXTime
  game <- newMVar $ GameState gameName False [] msgQ time
  forkIO $ startLoopGame game broadcastGameState
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

broadcastGameState :: GameState -> IO ()
broadcastGameState gameState = 
  let msg = toMessage gameState
      ps = players (gameState::GameState)
  in do
    forM_ ps $ \p@Player{..} -> send connection msg
    putStrLn $ "broadcasted..." ++ (show $ encode msg)

broadcast :: Message -> [Client] -> IO ()
broadcast msg clients = do
  putStrLn $ "broadcast sending: " ++ show msg
  forM_ clients $ \Client{..} -> send connection msg

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
