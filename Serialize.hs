module Serialize where

import State
import Views
import Text.JSON
import Control.Monad
import Control.Applicative.Error (maybeRead)
import Data.Label
import Control.Concurrent.STM

toPlayerAppearance :: Player -> PlayerAppearance
toPlayerAppearance player = PlayerAppearance (get playerName player)

instance JSON PlayerAppearance where
    readJSON json = do
        object <- readJSON json
        Just name <- valFromObj "name" object
        return (PlayerAppearance name)
    showJSON playerAppearance = 
        showJSON (toJSObject [
            ("name", showJSON $ get playerAppearanceName playerAppearance)
        ])

toSiteExterior :: Site -> SiteExterior
toSiteExterior site = SiteExterior (get siteName site) (get siteType site) (get sitePosition site)

instance JSON SiteExterior where
    readJSON json = do
        object <- readJSON json
        Just name <- valFromObj "name" object
        Just siteType <- valFromObj "type" object
        Just position <- valFromObj "position" object
        return (SiteExterior name siteType position)
    showJSON siteExterior = 
        showJSON (toJSObject [
            ("name", showJSON $ get siteExteriorName siteExterior),
            ("type", showJSON $ get siteExteriorType siteExterior),
            ("position", showJSON $ get siteExteriorPosition siteExterior)
        ])

toSiteInterior :: Site -> STM SiteInterior
toSiteInterior site = do
    guests <- mapM readTVar (get siteGuestVars site)
    return (SiteInterior (toSiteExterior site) (map (get playerName) guests))

instance JSON SiteInterior where
    readJSON json = do
        object <- readJSON json
        Just exterior <- valFromObj "exterior" object
        Just visitors <- valFromObj "visitors" object
        return (SiteInterior exterior visitors)
    showJSON siteInterior = 
        showJSON (toJSObject [
            ("exterior", showJSON $ get siteInteriorExterior siteInterior),
            ("visitors", showJSON $ get siteInteriorVisitors siteInterior)
        ])

instance JSON Place where
    readJSON json = msum [
            do 
                ("Street", position) <- readJSON json
                return (Street position),
            do 
                ("Inside", siteName) <- readJSON json
                return (Inside siteName)
        ]
    showJSON place = case place of
        Street position -> showJSON ("Street", position)
        Inside siteName -> showJSON ("Inside", siteName)

instance JSON Position where
    readJSON json = do
        object <- readJSON json
        Just x <- valFromObj "x" object
        Just y <- valFromObj "y" object
        return (Position x y)
    showJSON (Position x y) =
        showJSON $ toJSObject [
            ("x", x), ("y", y)
            ]

instance JSON Situation where
    readJSON json = msum [
            do 
                ["Idle"] <- readJSON json
                return Idle,
            do 
                ["Busted"] <- readJSON json
                return Busted,
            do 
                ("Trading", playerName) <- readJSON json
                return (Trading playerName)
        ]
    showJSON situation = case situation of
        Idle -> showJSON ["Idle"]
        Busted -> showJSON ["Busted"]
        Trading playerName -> showJSON ("Trading", playerName)

instance JSON SiteType where
    readJSON json = do
        [tag] <- readJSON json
        case maybeRead tag of
            Just value -> return value
            Nothing -> fail "Failed to read SiteType"
    showJSON siteType = showJSON [show siteType]

instance JSON DrugBag where
    readJSON json = do
        object <- readJSON json
        Just seller <- valFromObj "seller" object
        Just purity <- valFromObj "purity" object
        Just units <- valFromObj "units" object
        return (DrugBag seller purity units)
    showJSON drugBag = 
        showJSON (toJSObject [
            ("seller", showJSON $ get drugBagSeller drugBag),
            ("purity", showJSON $ get drugBagPurity drugBag),
            ("units", showJSON $ get drugBagUnits drugBag)
        ])

instance JSON Option where
    readJSON json = msum [
            do 
                ["TakeACap"] <- readJSON json
                return (TakeACap None),
            do 
                ("TakeACap", position) <- readJSON json
                return (TakeACap (Some position)),
            do 
                ("Enter", siteName) <- readJSON json
                return (Enter siteName),
            do 
                ["Exit"] <- readJSON json
                return Exit,
            do 
                ("DealDrugs", bagIndex) <- readJSON json
                return (DealDrugs (Some bagIndex)),
            do 
                ["DealDrugs"] <- readJSON json
                return (DealDrugs None),
            do 
                ("Trade", playerName) <- readJSON json
                return (Trade playerName),
            do 
                ["AbortTrade"] <- readJSON json
                return (AbortTrade),
            do 
                ("BribePolice", money) <- readJSON json
                return (BribePolice (Some money)),
            do 
                ["BribePolice"] <- readJSON json
                return (BribePolice None),
            do 
                ("SnitchFriend", playerName) <- readJSON json
                return (SnitchFriend playerName)
        ]
    showJSON option = case option of
        TakeACap None -> showJSON ["TakeACap"]
        TakeACap (Some position) -> showJSON ("TakeACap", position)
        Enter siteName -> showJSON ("Enter", siteName)
        Exit -> showJSON ["Exit"]
        DealDrugs None -> showJSON ["DealDrugs"]
        DealDrugs (Some index) -> showJSON ("DealDrugs", index)
        Trade playerName -> showJSON ("Trade", playerName)
        AbortTrade -> showJSON ["AbortTrade"]
        BribePolice None -> showJSON ["BribePolice"]
        BribePolice (Some money) -> showJSON ("BribePolice", money)
        SnitchFriend playerName -> showJSON ("SnitchFriend", playerName)

