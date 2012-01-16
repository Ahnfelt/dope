{-# LANGUAGE TemplateHaskell, TypeOperators, KindSignatures #-}
module State where

import Control.Monad
import Control.Category
import Control.Concurrent.STM
import Data.Label
import qualified Data.Map as Map
import Data.Map (Map)
import Prelude hiding ((.), id)

type Name = String

data Position = Position Int Int deriving (Show, Read, Eq)
type SiteName = String
type PlayerName = String

data Place
    = Street Position
    | Inside SiteName
    deriving (Show, Read, Eq)

data Situation
    = Idle
    | Busted
    | Trading PlayerName
    deriving (Show, Read, Eq)

data SiteType 
    = Club 
    | Jail
    deriving (Show, Read, Eq)

data DrugBag = DrugBag {
    _drugBagSeller :: PlayerName,
    _drugBagPurity :: Double,
    _drugBagUnits :: Int
    }
$(mkLabels [''DrugBag])

data Player = Player {
    _playerName :: String,
    _playerSituation :: Situation,
    _playerPlace :: Place,
    _playerMoney :: Integer,
    _playerDrugBags :: [DrugBag],
    _playerOnline :: Bool
    }
$(mkLabels [''Player])

data Site = Site {
    _siteName :: SiteName,
    _siteType :: SiteType,
    _sitePosition :: Position,
    _siteGuestVars :: [TVar Player]
    }
$(mkLabels [''Site])

data GameState = GameState {
    _statePlayerVars :: Map PlayerName (TVar Player),
    _stateSiteVars :: Map SiteName (TVar Site)
    }
$(mkLabels [''GameState])

modifyTVar var function = do
    value <- readTVar var
    writeTVar var (function value)

createPlayerVar :: TVar [TVar Player] -> Player -> STM (TVar Player)
createPlayerVar store player = do
    var <- newTVar player
    vars <- readTVar store
    writeTVar store (var : vars)
    return var

updatePlayer :: TVar Player -> (Player -> Player) -> STM Player
updatePlayer playerVar f = do
    player <- readTVar playerVar
    let player' = f player
    writeTVar playerVar player'
    return player'
       
getPlayersWhere :: [TVar Player] -> (Player -> Bool) -> STM [TVar Player]
getPlayersWhere playerVars predicate = 
    filterM (\var -> readTVar var >>= (return . predicate)) playerVars

getPlayerNames :: [TVar Player] -> STM [PlayerName]
getPlayerNames playerVars = do
    players <- mapM readTVar playerVars
    return (map (get playerName) players)

-- Maintains the invariant that a player is only in one place at a time,
-- even though this fact is represented in multiple places.
movePlayer :: GameState -> PlayerName -> Place -> STM ()
movePlayer state playerName place = do
    let Just playerVar = Map.lookup playerName (get statePlayerVars state)
    player <- readTVar playerVar
    case get playerPlace player of
        Inside siteName -> do
            let Just siteVar = Map.lookup siteName (get stateSiteVars state)
            modifyTVar siteVar (modify siteGuestVars (filter (/= playerVar)))
        Street _ -> return ()
    writeTVar playerVar (set playerPlace place player)
    case place of
        Inside siteName -> do
            let Just siteVar = Map.lookup siteName (get stateSiteVars state)
            modifyTVar siteVar (modify siteGuestVars (playerVar:))
        Street _ -> return ()

