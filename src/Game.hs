{-# LANGUAGE 
  OverloadedStrings,
  DeriveGeneric,
  RecordWildCards,
  FlexibleInstances,
  DuplicateRecordFields,
  DataKinds,
  GADTs  
#-}

module Game where

import Data.Aeson
import Data.Time
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, when, forM_)
import Control.Monad.IO.Class
import Models

tickMillis = 1000

type GameModifier = GameState -> NominalDiffTime -> GameState

ticker :: GameState -> [GameModifier] -> NominalDiffTime -> GameState
ticker game [] _ = game 
ticker game (mod:mods) t = ticker (mod game t) mods t

startGameTicker :: MVar GameState -> (GameState -> IO ()) -> IO ()
startGameTicker state callBack = do
  forkIO $ forever $ do
    threadDelay (tickMillis * 1000)
    tickGame state callBack
  return ()

tickGame :: MVar GameState -> (GameState -> IO ()) -> IO ()
tickGame state callBack = do
    tickTime <- getPOSIXTime
    gameState <- readMVar state
    let msgQ = queue gameState
    --msg <- readChan msgQ -- only handles one msg per loop, blocks
    msgs <- atomically $ flushTQueue msgQ 
    -- let msgMods = fmap msgToModifier allMsgs
    
    --putStrLn $ "reading message: " ++ (show (encode msg))
    let msgMods = fmap msgToModifier msgs
    let mods = [movePuck,scoreGame]
    let next = ticker gameState (msgMods ++ mods) tickTime
    
    liftIO $ modifyMVar_ state $ \s -> pure next
    postState <- readMVar state
    callBack next

movePuck :: GameModifier
movePuck g@GameState{..} t = 
  let
    nextPuckPos = addVec3 puckPos puckVel
  in
    g { puckPos = nextPuckPos }

scoreGame :: GameModifier
scoreGame g t = g

msgToModifier :: Message -> GameModifier
msgToModifier msg =
  case msg of
    MoveMsg playerName dir -> 
      let fdir = case dir of
                   Clockwise -> \x -> x-1
                   CounterClockwise -> \x -> x+1
      in
        \g t -> (g::GameState) 
                { players =  
                    fmap (\p -> 
                      if name (p::Player) == playerName 
                      then p {position = fdir (position p)}
                      else p
                    ) (players (g::GameState))
                }
    NewPlayerMsg gameName player ->
      \g t -> (g::GameState)
        {
          players = player : (players (g::GameState))
        }
    StartGameReqMsg ->
      \g t -> (g::GameState)
        {
          running = True
        }
    _ -> \g t -> g

gameMsgHandler :: TQueue Message -> MVar GameState -> IO ()
gameMsgHandler queue gameState = 
  forever $ do
    msg <- atomically $ readTQueue queue
    case msg of
      StartGameReqMsg -> do
        putStrLn $ "Game has been started..."
        liftIO $ modifyMVar_ gameState $ \s -> do
          return $ (s :: GameState) {running=True}
--        broadcastGameState gameState
      move@MoveMsg{..} -> do
        putStrLn $ "player " ++ show playerName ++ " moved " ++ show direction
        liftIO $ modifyMVar_ gameState $ \s -> do
          return $ movePlayer s move
 --       broadcastGameState gameState
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


