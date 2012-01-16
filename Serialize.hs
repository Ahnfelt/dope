module Serialize where

import State
import Views
import Text.JSON
import Control.Monad
import Control.Applicative.Error (maybeRead)
import Data.Label

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

