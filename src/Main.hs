{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Text (Text, pack)
import GHC.Generics

import qualified Network.WebSockets as WS


--The state of the server is represented as a collection of games.
type ServerState = [Game]

data Game = Game { gameId :: Int, clients :: [Client]}

data Client = Client {
      name :: Text
    , conn :: WS.Connection
    }

data PaddleDirection = Left | Right 
                     deriving (Show, Generic)

instance ToJSON PaddleDirection
instance FromJSON PaddleDirection

data ClientMessage 
    = InitMsg { clientName :: Text, cmsgGameId :: Maybe Int }
    | MoveMsg { clientName :: Text, direction:: PaddleDirection, distance :: Int }
    deriving (Show, Generic)

instance FromJSON ClientMessage where
    parseJSON = withObject "message or blob" $ \o -> asum [
        MoveMsg <$> o .: "clientName" <*> o .: "direction" <*> o .: "distance",
        InitMsg <$> o .: "clientName" <*> o .:? "gameId"]

instance ToJSON ClientMessage

data ServerMessage 
    = ServerMessage 
    { smsgGameId :: Int
    , msgType :: ServerMessageType 
    } deriving (Show, Generic)

instance ToJSON ServerMessage where
    toJSON o = object [
        "gameId" .= smsgGameId o,
        "type"   .= msgType o]

data ServerMessageType
    = GameCreated
    | GameNotCreated
    | ServerError
    deriving (Show, Generic)

instance ToJSON ServerMessageType

newServerState :: ServerState
newServerState = []

gameExists :: Int -> ServerState -> Bool
gameExists id = any (\g -> gameId g == id)

addPlayer :: Client -> Int -> ServerState -> ServerState
addPlayer c id games = fmap addPlayerToGame games
    where addPlayerToGame = (\g -> if gameId g == id then g {clients = c : (clients g)} else g)

addGame :: Game -> ServerState -> ServerState
addGame game games = game : games

getGame :: Int -> ServerState -> Game
getGame id games = head $ filter (\g -> gameId g == id) games

sbroadcast :: ServerMessage -> Game -> IO ()
sbroadcast message game = do
    putStrLn $ "message broadcasted: " ++ (show message)
    let cs = clients game
    forM_ cs $ \(Client _ conn) -> WS.sendTextData conn $ encode message

cbroadcast :: ClientMessage -> Game -> IO ()
cbroadcast message game = do
    putStrLn $ "message broadcasted: " ++ (show message)
    let cs = clients game
    forM_ cs $ \(Client _ conn) -> WS.sendTextData conn $ encode message


talk :: Client -> Game -> IO ()
talk (Client name conn) game = forever $ do
    jsonMsg <- WS.receiveData conn
    let msg = decode jsonMsg :: Maybe ClientMessage
    case msg of 
      Just (MoveMsg c dir dist) -> cbroadcast (MoveMsg c dir dist) game
      Nothing -> putStrLn $ "not broadcasted nothing..." ++ (show jsonMsg)
      Just (InitMsg c g) -> putStrLn $ "not broadcasted..." ++ (show jsonMsg)

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
    conn <- WS.acceptRequest pending
    games <- readMVar state
    WS.forkPingThread conn 30
    jsonMsg <- WS.receiveData conn
    case decode jsonMsg :: Maybe ClientMessage of
        Just (InitMsg c g) -> 
            case g of
                Just id | gameExists id games -> do
                              let newClient = Client c conn
                              liftIO $ modifyMVar_ state $ \s -> do
                                let s' = addPlayer newClient id games
                                return s'
                              putStrLn "game exists..."
                              WS.sendTextData conn ("Game exists!" :: Text)
                              talk newClient $ getGame id games 
                        | otherwise -> do
                              let newClient = Client c conn
                              let newGame = Game id [newClient]
                              liftIO $ modifyMVar_ state $ \s -> do
                                let s' = addGame newGame games
                                return s'
                              putStrLn "new game created!"
                              WS.sendTextData conn ("New game created!" :: Text)
                              talk newClient newGame
                Nothing -> do
                    let gid = length games
                    let newClient = (Client c conn)
                    let newGame = Game gid [newClient]
                    liftIO $ modifyMVar_ state $ \s -> do
                      let s' = addGame newGame games
                      putStrLn "new game created with a new id!"
                      WS.sendTextData conn $ encode $ ServerMessage gid GameCreated
                      return s'
                    talk newClient newGame
        Just (MoveMsg c dir dist) -> putStrLn "received move message!"
        _ -> do 
            putStrLn "received non-message message"
            WS.sendTextData conn $ encode $ ServerMessage 0 ServerError
