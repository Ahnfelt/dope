module Main where

import Network.Server
import Network.Socket
import System.IO
import Control.Monad
import Control.Applicative.Error (maybeRead) -- from package applicative-extras
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Label
import Prelude hiding ((.), id)

import Protocol
import Dope
import State
import ClientData

serveClient :: TVar GameState -> Maybe (TVar Player) -> (Handle, HostName, PortNumber) -> IO ()
serveClient stateVar playerVar (handle, host, port) = do
    case playerVar of 
        Nothing -> loginLoop
        Just playerVar -> inGameLoop playerVar

    where
        inGameLoop playerVar = do
            request <- listen
            case request of
                Act option -> do 
                    (error, player, possibilities) <- atomically $ do
                        error <- act playerVar option stateVar
                        player <- readTVar playerVar
                        state <- readTVar stateVar
                        possibilities <- options player state
                        return (error, toPlayerIntrospection player, possibilities)
                    respond $ case error of
                        Nothing -> OK player possibilities
                        Just reason -> Error (IllegalAct reason player possibilities)
                    inGameLoop playerVar
                Quit -> do
                    atomically $ do
                        modifyTVar playerVar (set playerOnline False)
                    respond Bye
                    
        loginLoop = do
            request <- listen
            case request of
                UsePlayer name -> do
                    state <- atomically $ readTVar stateVar
                    case Map.lookup name (get statePlayerVars state) of 
                        Just playerVar -> do
                            (player, possibilities) <- atomically $ do 
                                player <- readTVar playerVar
                                let player' = set playerOnline True player
                                writeTVar playerVar player'
                                possibilities <- options player' state
                                return (toPlayerIntrospection player', possibilities)
                            respond $ OK player possibilities
                            inGameLoop playerVar 
                        Nothing -> do 
                            respond (Error PlayerDoesNotExist)
                            loginLoop
                NewPlayer name -> do
                    playerVar <- atomically (newPlayerVar stateVar name)
                    case playerVar of 
                        Just playerVar -> do
                            (player, possibilities) <- atomically $ do 
                                player <- readTVar playerVar
                                state <- readTVar stateVar
                                possibilities <- options player state
                                return (toPlayerIntrospection player, possibilities)
                            respond $ OK player possibilities
                            inGameLoop playerVar
                        Nothing -> do 
                            respond (Error PlayerAlreadyExists)
                            loginLoop
                Act _ -> do
                    respond (Error NotLoggedIn)
                    loginLoop
                Quit -> respond Bye

            
    
        listen :: IO Request
        listen = do
            line <- hGetLine handle
            let trimmed = takeWhile (\c -> c /= '\n' && c /= '\r') line
            putStrLn ("From client " ++ show port ++ ": " ++ trimmed)
            case maybeRead trimmed of
                Just request -> return request
                Nothing -> do
                    respond (Error InvalidRequest)
                    listen

        respond :: Response -> IO ()
        respond response = do
            hPutStrLn handle (show response)
            hFlush handle
           


main :: IO ()
main = do 
    stateVar <- createWorld
    let port = 60000
    let ip = Server (SockAddrInet port (ipAddress (127, 0, 0, 1))) Stream (serveClient stateVar Nothing)
    serveMany Nothing [ip] >>= waitFor

