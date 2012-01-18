import Control.Applicative.Error (maybeRead)
import Control.Concurrent.MVar
import Control.Monad
import System.Exit
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.Maybe
import Data.Label
import Prelude hiding ((.), id)

import Protocol
import Dope
import State
import ClientData
import Client

type Client = Request -> IO Response

main :: IO ()
main = do
    let player = toPlayerIntrospection $ Player "John" Idle (Street (Position 1 2)) 1000 [DrugBag Crack "Kryster" 0.7 10] True
    stringClient <- createClient "localhost" 60000
    let client request = do 
        r <- stringClient (Just (show request))
        case r of
            Nothing -> exitSuccess
            Just stringResponse -> 
                case maybeRead stringResponse :: Maybe Response of
                    Nothing -> error $ "Failed to parse response from server: " ++ stringResponse
                    Just response -> return response
        
    --let client = Client' $ \request -> return (OK player [Exit, AbortTrade, SnitchFriend "Ahnfelt"], client)
    runGui client
    
----------------
-- The GUI

logoAttribute = Attr (SetTo bold) (SetTo bright_red) (SetTo black)
headingAttribute = Attr (SetTo bold) (SetTo bright_yellow) Default
valueAttribute = Attr (SetTo dim) (SetTo green) Default
borderAttribute = Attr Default (SetTo magenta) Default
drugBagAttribute = Attr (SetTo dim) (SetTo blue) Default
labelAttribute = Attr (SetTo bold) (SetTo bright_white) Default
playerLabelAttribute =  Attr (SetTo bold) (SetTo bright_white) Default

runGui :: Client -> IO ()
runGui client = do
    logoView <- newLogoView
    (playerView, updatePlayer) <- newPlayerView
    playerView <- withNormalAttribute borderAttribute playerView
    actionView <- newActionView client updatePlayer
    actionViewPadded <- withPadding (padLeftRight 4) actionView 
    
    layoutView <- newTable [column ColAuto] BorderNone
    setDefaultCellAlignment layoutView AlignCenter
    addRow layoutView logoView
    addRow layoutView actionViewPadded
    addRow layoutView playerView

    -- Create focus group with exit handler
    focusGroup <- newFocusGroup
    focusGroup `onKeyPressed` \_ key _ ->
        if key == KEsc then exitSuccess else return False
    addToFocusGroup focusGroup actionView
    addToFocusGroup focusGroup playerView
    setFocusGroupNextKey focusGroup (KASCII '\t') [] 

    -- Wrap the layout view in a collection and create a GUI runner
    collection <- newCollection
    _ <- addToCollection collection layoutView focusGroup
    runUi collection defaultContext
    

newLogoView :: IO (Widget Table)
newLogoView = do
    logo <- acsiiArtLogo dopeAscii1
    withNormalAttribute logoAttribute logo


newActionView :: Client -> (PlayerIntrospection -> IO ()) -> IO (Widget (Group Table))
newActionView client updatePlayer = do
    switcher <- newGroup
    let showLoginView = setCurrentGroupEntry switcher 0
    let showOptionView = setCurrentGroupEntry switcher 1
    let showErrorView = setCurrentGroupEntry switcher 2
    errorView <- plainText ""
    errorView `onKeyPressed` \_ key _ -> showOptionView >> return True
    
    let showError message = do
        setText errorView message
        showErrorView
    
    errorView <- wrapInTable errorView 
        
    let sendAndHandle request updateOptions = do
            response <- client request
            case response of
                OK player options -> do
                    updatePlayer player
                    updateOptions options
                    showOptionView
                Error (IllegalAct reason player options) -> do
                    updatePlayer player
                    updateOptions options
                    showError reason
                Error reason -> error (show reason)
                Bye -> exitSuccess
                
    (optionsView, updateOptions) <- do
        menu <- newList def_attr

        let updateOptions options = do
                clearList menu
                forM_ options $ \option -> do
                    item <- plainText (show option)
                    addToList menu option item

        onItemActivated menu $ \(ActivateItemEvent index option widget) -> 
            sendAndHandle (Act option) updateOptions
        table <- wrapWithLabel menu (Just ("Your options", labelAttribute))
        return (table, updateOptions)
        
    loginView <- do
        playerNameInputView <- editWidget
        onActivate playerNameInputView $ \this -> do
            playerName <- getEditText this
            sendAndHandle (NewPlayer playerName) updateOptions
        table <- wrapWithLabel playerNameInputView (Just ("Login with existing user", labelAttribute))
        return table
    
    _ <- addToGroup switcher loginView
    _ <- addToGroup switcher optionsView
    _ <- addToGroup switcher errorView
    showLoginView
    return switcher


newPlayerView :: IO (Widget Table, PlayerIntrospection -> IO ())
newPlayerView = do
    (statsView, updatePlayer) <- newStatsView
    (drugBagView, updateDrugBags) <- newDrugBagView

    playerView <- newTable [column ColAuto, column ColAuto] BorderFull
    setDefaultCellAlignment playerView AlignCenter
    addRow playerView $ statsView .|. drugBagView
    let updatePlayer' player = do
        updatePlayer player
        let p = get playerIntrospectionPlayer player
        updateDrugBags (get playerDrugBags p)
    relayAll playerView drugBagView
    return (playerView, updatePlayer')

    where
        newStatsView :: IO (Widget Table, PlayerIntrospection -> IO ())
        newStatsView = do 
            table <- newTable [column ColAuto, column ColAuto] BorderNone
            setDefaultCellAlignment table AlignLeft

            let emptyValue = plainText "" >>= withNormalAttribute valueAttribute            
            let label s = plainText s >>= withNormalAttribute headingAttribute

            nameLabel <- label "Name:"
            nameValue <- emptyValue
            addRow table $ nameLabel .|. nameValue            

            situationLabel <- label "Situation:"             
            situationValue <- emptyValue           
            addRow table $ situationLabel .|. situationValue            

            placeLabel <- label "Place:"             
            placeValue <- emptyValue          
            addRow table $ placeLabel .|. placeValue            

            moneyLabel <- label "Money:"             
            moneyValue <- emptyValue           
            addRow table $ moneyLabel .|. moneyValue            

            let updatePlayer player = do
                let p = get playerIntrospectionPlayer player
                setText nameValue (get playerName p)
                setText situationValue (show (get playerSituation p))
                setText placeValue (show (get playerPlace p))
                setText moneyValue (show (get playerMoney p) ++ "$")
            
            withLabel <- wrapWithLabel table (Just ("Your stats", playerLabelAttribute))
            return (withLabel, updatePlayer)

        newDrugBagView :: IO (Widget Table, [DrugBag] -> IO ())
        newDrugBagView = do
            list <- newStringList drugBagAttribute []
            let updateDrugBags drugBags = do
                clearList list
                forM_ drugBags $ \drugBag -> do
                    let drug = get drugBagDrug drugBag
                    let units = get drugBagUnits drugBag
                    let seller = get drugBagSeller drugBag
                    let putiry = get drugBagPurity drugBag
                    let line = show units ++ "g " ++ show drug ++ " from " ++ seller
                    textWidget <- plainText line     
                    addToList list line textWidget                       
            drugBagView <- wrapWithLabel list (Just ("Your drug bags", playerLabelAttribute))
            relayAll drugBagView list
            return (drugBagView, updateDrugBags)

wrapInTable :: Show a => Widget a -> IO (Widget Table)
wrapInTable widget = wrapWithLabel widget Nothing

wrapWithLabel :: Show a => Widget a -> (Maybe (String, Attr)) -> IO (Widget Table)
wrapWithLabel widget label = do
    fixedWidget <- boxFixed 40 12 widget
        >>= withPadding (padLeftRight 2)  
        >>= withPadding (padTopBottom 1)
    table <- newTable [column ColAuto] BorderNone
    setDefaultCellAlignment table AlignCenter
    case label of
        Just (labelText, labelAttribute) -> 
            plainText labelText 
                >>= withNormalAttribute labelAttribute 
                >>= withPadding (padTop 1)
                >>= addRow table
        Nothing -> return ()
    addRow table fixedWidget
    relayAll table widget
    return table

relayAll from to = do
    relayKeyEvents from to
    relayFocusEvents from to

--------------------
-- Utilities


acsiiArtLogo :: [String] -> IO (Widget Table)
acsiiArtLogo lines = do
    tableWidget <- newTable [column ColAuto] BorderNone
    setDefaultCellAlignment tableWidget AlignCenter
    forM_ lines $ \line -> do
        textWidget <- plainText line
        addRow tableWidget textWidget
    return tableWidget

dopeAscii1 = 
    [".__          "
    ,"|  \\ _ ._  _ "
    ,"|__/(_)[_)(/,"
    ,"       |     "]

dopeAscii2 = 
    ["  ____      U  ___ u  ____   U _____ u "
    ," |  _\"\\      \\/\"_ \\/U|  _\"\\ u\\| ___\"|/ "
    ,"/| | | |     | | | |\\| |_) |/ |  _|\"   "
    ,"U| |_| |\\.-,_| |_| | |  __/   | |___   "
    ," |____/ u \\_)-\\___/  |_|      |_____|  "
    ,"  |||_         \\\\    ||>>_    <<   >>  "
    ," (__)_)       (__)  (__)__)  (__) (__) "]

dopeAscii3 = 
    [" (                       "
    ," )\\ )                    "
    ,"(()/(                 (  "
    ," /(_))   (   `  )    ))\\ "
    ,"(_))_    )\\  /(/(   /((_)"
    ," |   \\  ((_)((_)_\\ (_))  "
    ," | |) |/ _ \\| '_ \\)/ -_) "
    ," |___/ \\___/| .__/ \\___| "
    ,"            |_|          "]

dopeAscii4 = 
    ["   ______             "
    ,"  (, /    )           "
    ,"    /    / _____    _ "
    ,"  _/___ /_(_) /_)__(/_"
    ,"(_/___ /   .-/        "
    ,"          (_/         "]
    
    