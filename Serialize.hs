module Serialize where

import Text.JSON
import Control.Monad
import Control.Applicative.Error (maybeRead)
import Data.Label

import State
import ClientData
import Protocol

instance JSON PlayerAppearance where
    readJSON json = do
        object <- readJSON json
        Just name <- valFromObj "name" object
        return (PlayerAppearance name)
    showJSON playerAppearance = 
        showJSON (toJSObject [
            ("name", showJSON $ get playerAppearanceName playerAppearance)
        ])

instance JSON PlayerIntrospection where
    readJSON json = do
        object <- readJSON json
        Just name <- valFromObj "name" object
        Just situation <- valFromObj "situation" object
        Just place <- valFromObj "place" object
        Just money <- valFromObj "money" object
        Just drugBags <- valFromObj "drugBags" object
        Just online <- valFromObj "online" object
        return (PlayerIntrospection $ Player name situation place money drugBags online)
    showJSON (PlayerIntrospection player) = 
        showJSON (toJSObject [
            ("name", showJSON $ get playerName player),
            ("situation", showJSON $ get playerSituation player),
            ("place", showJSON $ get playerPlace player),
            ("money", showJSON $ get playerMoney player),
            ("drugBags", showJSON $ get playerDrugBags player),
            ("online", showJSON $ get playerOnline player)
        ])

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
        Just drug <- valFromObj "drug" object
        Just seller <- valFromObj "seller" object
        Just purity <- valFromObj "purity" object
        Just units <- valFromObj "units" object
        return (DrugBag drug seller purity units)
    showJSON drugBag = 
        showJSON (toJSObject [
            ("drug", showJSON $ get drugBagDrug drugBag),
            ("seller", showJSON $ get drugBagSeller drugBag),
            ("purity", showJSON $ get drugBagPurity drugBag),
            ("units", showJSON $ get drugBagUnits drugBag)
        ])

instance JSON Drug where
    readJSON json = do
        [tag] <- readJSON json
        case maybeRead tag of
            Just value -> return value
            Nothing -> fail "Failed to read Drug"
    showJSON drug = showJSON [show drug]

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


instance JSON Request where
    readJSON json = msum [
            do 
                ("UsePlayer", playerName) <- readJSON json
                return (UsePlayer playerName),
            do 
                ("NewPlayer", playerName) <- readJSON json
                return (NewPlayer playerName),
            do 
                ("Act", option) <- readJSON json
                return (Act option),
            do 
                ["Quit"] <- readJSON json
                return Quit
        ]
    showJSON request = case request of
        UsePlayer playerName -> showJSON ("UsePlayer", playerName)
        NewPlayer playerName -> showJSON ("NewPlayer", playerName)
        Act option -> showJSON ("Act", option)
        Quit -> showJSON ["Quit"]

instance JSON Response where
    readJSON json = msum [
            do 
                ("OK", playerIntrospection, options) <- readJSON json
                return (OK playerIntrospection options),
            do 
                ("Error", error) <- readJSON json
                return (Protocol.Error error),
            do 
                ["Bye"] <- readJSON json
                return Bye
        ]
    showJSON response = case response of
        OK playerIntrospection options -> showJSON ("OK", playerIntrospection, options)
        Protocol.Error error -> showJSON ("Error", error)
        Bye -> showJSON ["Bye"]

instance JSON Protocol.Error where
    readJSON json = msum [
            do 
                ["InvalidRequest"] <- readJSON json
                return InvalidRequest,
            do 
                ("IllegalAct", error, playerIntrospection, options) <- readJSON json
                return (IllegalAct error playerIntrospection options),
            do 
                ["PlayerDoesNotExist"] <- readJSON json
                return PlayerDoesNotExist,
            do 
                ["NotLoggedIn"] <- readJSON json
                return NotLoggedIn,
            do 
                ["PlayerAlreadyExists"] <- readJSON json
                return PlayerAlreadyExists
        ]
    showJSON response = case response of
        InvalidRequest -> showJSON ["InvalidRequest"]
        IllegalAct error player options -> showJSON ("IllegalAct", error, player, options)
        PlayerDoesNotExist -> showJSON ["PlayerDoesNotExist"]
        NotLoggedIn -> showJSON ["NotLoggedIn"]
        PlayerAlreadyExists -> showJSON ["PlayerAlreadyExist"]

