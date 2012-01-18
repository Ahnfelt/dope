{-# LANGUAGE TemplateHaskell, TypeOperators, KindSignatures #-}
module State where

import Control.Monad
import Control.Category
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Label
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

data Drug
    = Heroin
    | Cocaine
    | Meth
    | Crack
    | LSD
    | Ecstasy
    | Opium
    | Marijuana
    | Mushrooms
    deriving (Show, Read, Eq)

data DrugBag = DrugBag {
    _drugBagDrug :: Drug,
    _drugBagSeller :: PlayerName,
    _drugBagPurity :: Double,
    _drugBagUnits :: Int
    } deriving (Show, Read, Eq)
$(mkLabels [''DrugBag])

data Player = Player {
    _playerName :: String,
    _playerSituation :: Situation,
    _playerPlace :: Place,
    _playerMoney :: Integer,
    _playerDrugBags :: [DrugBag],
    _playerOnline :: Bool
    } deriving (Show, Read, Eq)
$(mkLabels [''Player])

data SiteType 
    = Club 
    | Jail
    deriving (Show, Read, Eq)

data Site = Site {
    _siteName :: SiteName,
    _siteType :: SiteType,
    _sitePosition :: Position,
    _siteGuestVars :: [TVar Player]
    } deriving (Eq)
$(mkLabels [''Site])

data GameState = GameState {
    _statePlayerVars :: Map PlayerName (TVar Player),
    _stateSiteVars :: Map SiteName (TVar Site)
    }
$(mkLabels [''GameState])

modifyTVar var function = do
    value <- readTVar var
    writeTVar var (function value)

filterVars :: [TVar a] -> (a -> Bool) -> STM [TVar a]
filterVars vars predicate = 
    filterM (\var -> readTVar var >>= (return . predicate)) vars

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

createWorld :: IO (TVar GameState)
createWorld = atomically $ do
    let jail = Site "The Prison" Jail (Position 0 0) []
    let smokey = Site "Smokey Joe's" Club (Position 4 4) []
    siteVars <- makeSiteVars [jail, smokey]
    newTVar $ GameState Map.empty siteVars
    

makeSiteVars :: [Site] -> STM (Map SiteName (TVar Site))
makeSiteVars sites = do
    pairs <- forM sites (\s -> do 
        var <- newTVar s
        return (get siteName s, var))
    return $ Map.fromList pairs


getJail :: [TVar Site] -> STM Site
getJail siteVars = do
    hits <- filterVars siteVars (\site -> get siteType site == Jail)
    case hits of
        [] -> error "No jail found"
        siteVar : _ -> readTVar siteVar
            

-- | Create a new player and put her into the nearest jail. 
-- | Returns Nothing if the requested player name is unavailable.
newPlayerVar :: TVar GameState -> String -> STM (Maybe (TVar Player))
newPlayerVar stateVar name = do
    state <- readTVar stateVar
    let names = Map.keys (get statePlayerVars state)
    if elem name names
        then return Nothing
        else do
            let siteVars = (Map.elems (get stateSiteVars state))
            jail <- getJail siteVars
            let jailPosition = get sitePosition jail
            let player = Player name Idle (Street jailPosition) 100 [] True
            var <- newTVar player
            let playerVars = Map.insert name var (get statePlayerVars state)
            let state' = set statePlayerVars playerVars state
            writeTVar stateVar state'
            return (Just var)
            




