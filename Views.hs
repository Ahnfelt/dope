{-# LANGUAGE TemplateHaskell #-}
module Views where

import State
import Data.Label

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

data SiteExterior = SiteExterior {
    _siteExteriorName :: String,
    _siteExteriorType :: SiteType,
    _siteExteriorPosition :: Position
    }
$(mkLabels [''SiteExterior])

data SiteInterior = SiteInterior {
    _siteInteriorExterior :: SiteExterior,
    _siteInteriorVisitors :: [PlayerName]
    }
$(mkLabels [''SiteInterior])

