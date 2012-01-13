module PlayerStore where

import Control.Concurrent.STM
import Control.Monad

type Name = String
type PlayerHandle a = TVar a
type PlayerStore a = TVar [PlayerHandle a]

newPlayerStore :: IO (PlayerStore a)
newPlayerStore = newTVarIO []

createPlayer :: PlayerStore a -> a -> IO (PlayerHandle a)
createPlayer store player = atomically $ do
    var <- newTVar player
    vars <- readTVar store
    writeTVar store (var : vars)
    return var

updatePlayer :: PlayerHandle a -> (a -> a) -> IO a
updatePlayer handle f = atomically $ do
    player <- readTVar handle
    let player' = f player
    writeTVar handle player'
    return player'
       
readPlayer :: PlayerHandle a -> IO a
readPlayer = readTVarIO

getPlayersWhere :: PlayerStore a -> (a -> Bool) -> IO [(a, PlayerHandle a)]
getPlayersWhere store predicate = atomically $ do
    vars <- readTVar store
    players <- forM vars (\var -> do
        player <- readTVar var
        return (player, var))
    return (filter (predicate . fst) players)
    
