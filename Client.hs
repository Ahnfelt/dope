module Client where

import Network.Socket

createClient :: HostName -> Int -> IO (Maybe String -> IO (Maybe String))
createClient host port = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    let client message = case message of
            Nothing -> do 
                sClose sock
                return Nothing
            Just message -> do
                bytesSent <- send sock (message ++ "\n") -- TODO check byted sent
                received <- recv sock 1024 -- TODO buffer size
                let trimmed = takeWhile (\c -> c /= '\n' && c /= '\r') received
                -- TODO handle socket closed and return Nothing
                return (Just trimmed)
    return client
    

