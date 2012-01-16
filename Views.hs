{-# LANGUAGE TemplateHaskell #-}
module Views where

import State
import Data.Label

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

