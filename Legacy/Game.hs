{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Game where

import Control.Monad
import System.Random
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import qualified PlayerStore as PS

data Place 
    = Jail
    | Street
    | Club
    deriving (Show, Read, Eq)

data Situation
    = At Place
    | Busted Place
    deriving (Show, Read, Eq)

data Option
    = Goto Place
    | SellDrugs
    | BribePolice
    | SnitchFriend
    deriving (Show, Read, Eq)
    
data Player = Player {
    _playerName :: String,
    _playerSituation :: Situation,
    _playerOnline :: Bool
} deriving (Show, Read, Eq)

$(mkLabels [''Player])

type PlayerStore = PS.PlayerStore Player
type PlayerHandle = PS.PlayerHandle Player

type Game = IO

newPlayerStore :: Game PlayerStore
newPlayerStore = PS.newPlayerStore

createPlayer :: PlayerStore -> String -> Game (Maybe (Player, PlayerHandle, [Option]))
createPlayer store name = do
    let situation = At Jail
    let player = Player name situation True
    let options = possibilities player
    handle <- PS.createPlayer store player
    return (Just (player, handle, options))


login :: PlayerStore -> String -> Game (Maybe (Player, PlayerHandle, [Option]))
login store name = do
    players <- PS.getPlayersWhere store (\p -> get playerName p == name)
    case players of
        ((player, handle) : _) -> do
            PS.updatePlayer handle (set playerOnline True)
            let options = possibilities player
            return (Just (player, handle, options))
        _ -> return Nothing
        
logout :: PlayerHandle -> Game ()
logout playerHandle = do 
    PS.updatePlayer playerHandle (set playerOnline False)
    return ()


possibilities :: Player -> [Option]
possibilities player = case get playerSituation player of
    At Jail -> [Goto Street]
    At Street -> [SellDrugs, Goto Club]
    At Club -> [Goto Street]
    Busted location -> [Goto Jail, BribePolice, SnitchFriend]

act :: PlayerStore -> PlayerHandle -> Option -> Game (Either Player Player)
act store handle option = do
    player <- PS.readPlayer handle
    let situation = get playerSituation player
    if elem option (possibilities player)
        then do
            case (option, situation) of
                (Goto place, _) -> returnTo place
                (SellDrugs, At Street) -> returnTo Street
                (BribePolice, Busted location) -> do
                    r <- randomIO :: IO Double
                    if r < 0.2 
                        then returnTo Jail
                        else returnTo location
                (SnitchFriend, Busted location) -> do
                    r <- randomIO :: IO Double
                    if r < 0.8 
                        then returnTo Jail
                        else returnTo location
                (option, situation) -> do
                    -- TODO find a static varifiable solution
                    putStrLn ("Server Error - cannot handle the option " ++ show option 
                        ++ "in the situation " ++ show situation)
                    returnTo Jail
        else return (Left player)
    where
    returnTo location = do
         player <- PS.updatePlayer handle (set playerSituation (At location))
         return (Right player)

