module Serialize where

import State
import Text.JSON
import Control.Monad

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
        showJSON [("x", x), ("y", y)]

