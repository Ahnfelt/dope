module Main where

import Network.Server
import Network.Socket
import System.IO
import Control.Monad
import Game
import Control.Applicative.Error (maybeRead) -- from package applicative-extras

data Request
    = UsePlayer String
    | NewPlayer String
    | Act Option
    | Quit
    deriving (Show, Read, Eq)
    
data Response
    = OK Player [Option]
    | Error Error
    | Bye
    deriving (Show, Read, Eq)

data Error
    = InvalidRequest
    | IlligalAct Player [Option]
    | PlayerDoesNotExist
    | NotLoggedIn
    | PlayerAlreadyExist
    deriving (Show, Read, Eq)


serveClient :: PlayerStore -> Maybe PlayerHandle -> (Handle, HostName, PortNumber) -> IO ()
serveClient playerStore playerHandle (handle, host, port) = do
    case playerHandle of 
        Nothing -> loginLoop
        Just playerHandle -> inGameLoop playerHandle

    where
        inGameLoop playerHandle = do
            request <- listen
            case request of
                Act option -> do 
                    either <- act playerStore playerHandle option
                    respond $ case either of
                        Right player -> OK player (possibilities player)
                        Left player -> Error (IlligalAct player (possibilities player))
                    inGameLoop playerHandle
                Quit -> do
                    logout playerHandle
                    respond Bye
                    
        loginLoop = do
            request <- listen
            case request of
                UsePlayer name -> do
                    m <- login playerStore name
                    case m of 
                        Just (player, handle, options) -> do
                            respond $ OK player options
                            inGameLoop handle 
                        Nothing -> do 
                            respond (Error PlayerDoesNotExist)
                            loginLoop
                NewPlayer name -> do
                    m <- createPlayer playerStore name
                    case m of 
                        Just (player, handle, options) -> do
                            respond $ OK player options
                            inGameLoop handle 
                        Nothing -> do 
                            respond (Error PlayerAlreadyExist)
                            loginLoop
                Act _ -> do
                    respond (Error NotLoggedIn)
                    loginLoop
                Quit -> respond Bye

            
    
        listen :: IO Request
        listen = do
            line <- hGetLine handle
            let trimmed = takeWhile (\c -> c /= '\n' && c /= '\r') line
            putStrLn ("From client (" ++ show (length trimmed) ++ ")" ++ show port ++ ": " ++ trimmed)
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
    playerStore <- newPlayerStore
    let port = 60000
    let ip =  Server (SockAddrInet port (ipAddress (127, 0, 0, 1))) Stream (serveClient playerStore Nothing)
    serveMany Nothing [ip] >>= waitFor
    
    
