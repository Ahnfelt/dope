import Control.Concurrent.MVar
import Control.Monad
import System.Exit
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.Label
import Prelude hiding ((.), id)

import Protocol
import Dope
import State

data Client' = Client' (Request -> IO (Response, Client'))

main :: IO ()
main = do
    let player = Player "John" Idle (Street (1,2)) 1000 [DrugBag Crack "Kryster" 0.7 10] True

    let client = Client' $ \request -> do 
            return (OK player [Exit], client)
    runGui client
    
----------------
-- The GUI


runGui :: Client' -> IO ()
runGui client = do
    logoView <- newLogoView
    (playerView, updatePlayer) <- newPlayerView
    actionView <- newActionView client updatePlayer
    
    layoutView <- newTable [column ColAuto] BorderFull
    setDefaultCellAlignment layoutView AlignCenter
    addRow layoutView logoView
    addRow layoutView actionView
    addRow layoutView playerView

    -- Create focus group with exit handler
    focusGroup <- newFocusGroup
    focusGroup `onKeyPressed` \_ key _ ->
        if key == KEsc then exitSuccess else return False
    addToFocusGroup focusGroup actionView
    setFocusGroupNextKey focusGroup (KASCII '\t') [] 

    -- Wrap the layout view in a collection and create a GUI runner
    collection <- newCollection
    _ <- addToCollection collection layoutView focusGroup
    runUi collection defaultContext{skin=unicodeRoundedSkin}
    

newLogoView :: IO (Widget Table)
newLogoView = acsiiArtLogo dopeAscii1


newActionView :: Client' -> (Player -> IO ()) -> IO (Widget (Group Table))
newActionView (Client' client) updatePlayer = do
    switcher <- newGroup
    let showLoginView = setCurrentGroupEntry switcher 0
    let showOptionView = setCurrentGroupEntry switcher 1

    let sendAndHandle request updateOptions = do
            (response, newClient) <- client request
            case response of
                OK player options -> do 
                    updatePlayer player
                    updateOptions options
                    showOptionView
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
        table <- wrapInTable menu
        return (table, updateOptions)
        
    loginView <- do
        playerNameInputView <- editWidget
        onActivate playerNameInputView $ \this -> do
            playerName <- getEditText this
            sendAndHandle (NewPlayer playerName) updateOptions
        table <- wrapInTable playerNameInputView
        return table
    
    _ <- addToGroup switcher loginView
    _ <- addToGroup switcher optionsView
    showLoginView
    return switcher


newPlayerView :: IO (Widget Table, Player -> IO ())
newPlayerView = do
    (statsView, updatePlayer) <- newStatsView
    (drugBagView, updateDrugBag) <- newDrugBagView

    playerView <- newTable [column ColAuto, column ColAuto] BorderFull
    setDefaultCellAlignment playerView AlignCenter
    addRow playerView $ statsView .|. drugBagView
    return (playerView, updatePlayer)

    where
        newStatsView :: IO (Widget Table, Player -> IO ())
        newStatsView = do 
            table <- newTable [column ColAuto, column ColAuto] BorderNone
            setDefaultCellAlignment table AlignLeft
    
            nameLabel <- plainText "Name:"             
            nameValue <- plainText ""
            addRow table $ nameLabel .|. nameValue            

            situationLabel <- plainText "Situation:"             
            situationValue <- plainText ""             
            addRow table $ situationLabel .|. situationValue            

            placeLabel <- plainText "Place:"             
            placeValue <- plainText ""             
            addRow table $ placeLabel .|. placeValue            

            moneyLabel <- plainText "money:"             
            moneyValue <- plainText ""             
            addRow table $ moneyLabel .|. moneyValue            

            let updatePlayer player = do
                setText nameValue (get playerName player)
                setText situationValue (show (get playerSituation player))
                setText placeValue (show (get playerPlace player))
                setText moneyValue (show (get playerMoney player) ++ "$")

            return (table, updatePlayer)

        newDrugBagView :: IO (Widget Table, Player -> IO ())
        newDrugBagView = do
            drugBagView <- plainText "DRUG BAG"
            w <- wrapInTable drugBagView
            return (w, undefined)

wrapInTable :: Show a => Widget a -> IO (Widget Table)
wrapInTable widget = do
    fixedWidget <- boxFixed 20 8 widget
    table <- newTable [column ColAuto] BorderNone
    setDefaultCellAlignment table AlignCenter
    addRow table fixedWidget
    relayKeyEvents table widget
    relayFocusEvents table widget
    return table

--------------------
-- Utilities


acsiiArtLogo :: [String] -> IO (Widget Table)
acsiiArtLogo lines = do
    tableWidget <- newTable [column ColAuto] BorderNone
    setDefaultCellAlignment tableWidget AlignCenter
    forM_ lines $ \line -> do
        textWidget <- plainText line
        colored <- withNormalAttribute (fgColor blue) textWidget
        addRow tableWidget colored
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
    
    