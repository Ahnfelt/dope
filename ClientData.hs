{-# LANGUAGE TemplateHaskell #-}
module ClientData where

import State
import Data.Label
import Control.Concurrent.STM

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

data PlayerAppearance = PlayerAppearance {
    _playerAppearanceName :: String
    }
$(mkLabels [''PlayerAppearance])

toPlayerAppearance :: Player -> PlayerAppearance
toPlayerAppearance player = PlayerAppearance (get playerName player)

data SiteExterior = SiteExterior {
    _siteExteriorName :: String,
    _siteExteriorType :: SiteType,
    _siteExteriorPosition :: Position
    }
$(mkLabels [''SiteExterior])

toSiteExterior :: Site -> SiteExterior
toSiteExterior site = SiteExterior (get siteName site) (get siteType site) (get sitePosition site)

data SiteInterior = SiteInterior {
    _siteInteriorExterior :: SiteExterior,
    _siteInteriorVisitors :: [PlayerName]
    }
$(mkLabels [''SiteInterior])

toSiteInterior :: Site -> STM SiteInterior
toSiteInterior site = do
    guests <- mapM readTVar (get siteGuestVars site)
    return (SiteInterior (toSiteExterior site) (map (get playerName) guests))

