{-# LANGUAGE TemplateHaskell, TypeOperators, KindSignatures, ScopedTypeVariables #-}
module Dope where

import Control.Monad
import Control.Concurrent.STM
import System.Random
import Control.Category
import Data.Label
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (find)
import Prelude hiding ((.), id)

import State

data Optional a = Some a | None deriving (Show, Read)

instance Eq a => Eq (Optional a) where
    _ == _ = True

data Option 
    = TakeACap (Optional Position)
    | Enter SiteName
    | Exit
    | DealDrugs (Optional Int)
    | Trade PlayerName
    | AbortTrade
    | BribePolice (Optional Integer)
    | SnitchFriend PlayerName
    deriving (Show, Read, Eq)

options :: Player -> GameState -> STM [Option]
options player state =
    case (get playerSituation player, get playerPlace player) of
        (Idle, place) -> 
            case place of 
                Street position -> do
                    sites <- mapM readTVar (Map.elems (get stateSiteVars state))
                    let site = case find (\s -> get sitePosition s == position) sites of
                            Just site -> [Enter (get siteName site)]
                            Nothing -> []
                    return (TakeACap None : DealDrugs None : site)
                Inside siteName -> do
                    case Map.lookup siteName (get stateSiteVars state) of
                        Just siteVar -> do
                            site <- readTVar siteVar
                            case get siteType site of
                                Jail -> return []
                                Club -> do
                                    guestNames <- getPlayerNames (get siteGuestVars site)
                                    return (Exit : map Trade guestNames)
                        Nothing -> return []
        (Busted, place) -> do
            let vendorNames = map (get drugBagSeller) (get playerDrugBags player)
            return (BribePolice None : map SnitchFriend vendorNames)
        (Trading otherPlayer, place) -> return [AbortTrade]
    
act :: TVar Player -> Option -> TVar GameState -> STM (Maybe String)
act playerVar option stateVar = do
    player <- readTVar playerVar
    state <- readTVar stateVar
    possibilities <- options player state
    if elem option possibilities
        then do 
            case option of
                TakeACap (Some destination) -> do
                    let Street origin = get playerPlace player
                    let expense = taxameter origin destination
                    let money = get playerMoney player
                    if money >= expense
                        then do
                            let player' = set playerPlace (Street destination) $ 
                                    set playerMoney (money - expense) player
                            writeTVar playerVar player'
                            return Nothing
                        else return $ Just "You ain't got the money"
                TakeACap None -> return $ Just "Take a cap, where to"
                Enter siteName -> do
                    movePlayer state (get playerName player) (Inside siteName)
                    return Nothing
                Exit -> do
                    let Inside siteName = get playerPlace player
                    case Map.lookup siteName (get stateSiteVars state) of
                        Just siteVar -> do
                            site <- readTVar siteVar
                            movePlayer state (get playerName player) (Street (get sitePosition site))
                            return Nothing
                        Nothing -> error "Error in options - site should exist"
                DealDrugs (Some index) -> do
                    let drugBags = get playerDrugBags player
                    if index < 0 || index >= length drugBags
                        then return $ Just "Stop hallucinating."
                        else do
                            let drugBag = drugBags !! index
                            let units = get drugBagUnits drugBag
                            let drugBag' = set drugBagUnits (units - 1) drugBag
                            when (units < 1) $ error "Nothing left in the bag of drugs to sell"
                            let drugBags' = if units == 1
                                    then take units drugBags ++ drop (units + 1) drugBags
                                    else take units drugBags ++ [drugBag'] ++ drop (units + 1) drugBags
                            writeTVar playerVar (set playerDrugBags drugBags' player)
                            return Nothing
                DealDrugs None -> return $ Just "How much though?"
                Trade partnerName -> return $ Just "Trading is not yet implemented"
                AbortTrade -> return $ Just "Trading is not yet implemented"
                BribePolice (Some money) -> do
                    movePlayer state (get playerName player) (Inside "Jail")
                    return Nothing
                BribePolice None -> return $ Just "I wonder how much dough will turn the tides..."
                SnitchFriend friendName -> do
                    movePlayer state (get playerName player) (Inside "Jail")
                    return Nothing
        else return $ Just "Option unavailable"

taxameter :: Position -> Position -> Integer 
taxameter (Position x1 y1) (Position x2 y2) = fromIntegral $ abs (x2 - x1) + abs (y2 - y1)

