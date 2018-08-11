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
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad (forever, when, forM_)
import Control.Monad.IO.Class
import Models


ticker :: GameState -> [(GameState -> GameState)] -> GameState
ticker game [] = game 
ticker game (mod:mods) = ticker (mod game) mods

--tickMods :: [(GameState -> GameState)]
--tickMods = [movePuck,scoreGame]

--movePaddles :: GameState -> GameState
--movePaddles game = {

startLoopGame :: MVar GameState -> (GameState -> IO ()) -> IO ()
startLoopGame state callBack = forever $ do
    gameState <- readMVar state
    let msgQ = queue gameState
    msg <- readChan msgQ -- only handles one msg per loop
    -- let msgMods = fmap msgToModifier allMsgs
    
    --putStrLn $ "reading message: " ++ (show (encode msg))
    let msgMod = msgToModifier msg
    let mods = [movePuck,scoreGame]
    let next = ticker gameState (msgMod : mods)
    
    liftIO $ modifyMVar_ state $ \s -> pure next
    postState <- readMVar state
    callBack next

movePuck :: GameState -> GameState
movePuck g = g

scoreGame :: GameState -> GameState
scoreGame g = g

msgToModifier :: Message -> (GameState -> GameState)
msgToModifier msg =
  case msg of
    MoveMsg playerName dir -> 
      let fdir = case dir of
                   Clockwise -> \x -> x-1
                   CounterClockwise -> \x -> x+1
      in
        \g -> (g::GameState) 
                { players =  
                    fmap (\p -> 
                      if name (p::Player) == playerName 
                      then p {position = fdir (position p)}
                      else p
                    ) (players (g::GameState))
                }
    NewPlayerMsg gameName player ->
      \g -> (g::GameState)
        {
          players = player : (players (g::GameState))
        }
    _ -> \g -> g

gameMsgHandler :: Chan Message -> MVar GameState -> IO ()
gameMsgHandler chan gameState = 
  forever $ do
    msg <- readChan chan
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


