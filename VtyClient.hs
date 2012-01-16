import Control.Concurrent.MVar
import Control.Monad
import System.Exit
import Graphics.Vty
import Graphics.Vty.Widgets.All

import Protocol

main :: IO ()
main = do
    let client = undefined
    runGui client
    

type Client' = Request -> IO (Response, Client')

----------------
-- The GUI


runGui :: Client' -> IO ()
runGui client = do
    logoView <- newLogoView
    (playerView, updatePlayer) <- newPlayerView
    actionView <- newActionView act updatePlayer
    
    layoutView <- newTable [column ColAuto] BorderFull
    setDefaultCellAlignment layoutView AlignCenter
    addRow layoutView logoView
    addRow layoutView actionView
    addRow layoutView playerView

    -- Create focus group with exit handler
    focusGroup <- newFocusGroup
    focusGroup `onKeyPressed` \_ key _ ->
        if key == KEsc || key == KASCII 'q' then exitSuccess else return False
    addToFocusGroup focusGroup actionView

    -- Wrap the layout view in a collection and create a GUI runner
    collection <- newCollection
    _ <- addToCollection collection layoutView focusGroup
    runUi collection defaultContext{skin=unicodeRoundedSkin}
    

newLogoView :: IO (Widget Table)
newLogoView = acsiiArtLogo dopeAscii1


newActionView :: Client' -> ([Player] -> IO ()) -> IO (Widget (Group Table))
newActionView client updatePlayer = do
    let choose option = do
        


    (optionsView, updateOptions) <- newOptionView choose 
    switcher <- newGroup
    switchToOptionView <- addToGroup switcher mainMenu

    let login playerName = do
        response <- client (NewPlayer playerName)
        case response of
            OK player options -> do 
                updatePlayer player
                updateOptions options
                switchToOptionView
            Error reason -> error (show PlayerAlreadyExist) -- TODO handle PlayerAlreadyExist etc.
            Bye -> exitSuccess

                
        
    loginView <- newLoginView login
    switchToLoginView <- addToGroup switcher loginView


    where
        newLoginView login = do
            playerNameInputView <- editWidget
            onActivate playerNameInputView $ \this ->
                playerName <- getEditText this
                login playerName
            
        newOptionView choose = do
            menu <- newList def_attr
            let updateOptions options = do
                clearList menu
                forM_ options $ \option ->
                    item <- plainText (show option)
                    addToList menu option item
            onItemActivated menu $ \(ActivateItemEvent index option widget) -> choose option
            return (menu, updateOptions)


newPlayerView :: IO (Widget Table, Player -> IO ())
newPlayerView = undefined
    where
        newStatsView :: IO (Widget Table)
        newStatsView = undefined

        newDrugBagView :: IO (Widget Table)
        newDrugBagView = undefined



menuTest :: IO ()    
menuTest = do    

    -- Main menu
    mainMenu <- newStringList def_attr ["Deal drugs", "Second Item", "The Last!"]

    -- Deal drugs menu
    dealMenu <- newStringList def_attr ["Sell 1 gram coke [from Kryster]", "Sell 1 drop LSD [from player 432]"]
    
    menuSwitcher <- newGroup
    switchToMainMenu <- addToGroup menuSwitcher mainMenu
    switchToDealMenu <- addToGroup menuSwitcher dealMenu
    
    -- Handler for main menu
    onItemActivated mainMenu $ \(ActivateItemEvent index payload widget) -> do
        setText widget ("* " ++ payload)
        switchToDealMenu

    -- Handler for deal menu
    onItemActivated dealMenu $ \(ActivateItemEvent index payload widget) -> do
        setText widget ("* " ++ payload)
        switchToMainMenu

    -- Title widget
    titleWidget <- acsiiArtLogo dopeAscii1
    
    -- Shows where we are in the menu tree
    pathWidget <- plainText "path ... " >>= withNormalAttribute (fgColor green)
    
    -- Fixing the size of the menu widget
    fixedWidget <- boxFixed 40 8 menuSwitcher

    -- Adding borders to the menu widget
    hPaddedWidget <- padded fixedWidget (padLeftRight 2) 
    paddedWidget <- padded hPaddedWidget (padTopBottom 1) 
    borderWidget <- bordered paddedWidget
    
    -- Using a table widget for layout
    tableWidget <- newTable [column ColAuto] BorderFull
    setDefaultCellAlignment tableWidget AlignCenter
    addRow tableWidget titleWidget
    addRow tableWidget pathWidget
    addRow tableWidget borderWidget

    -- Create focus group with exit handler
    focusGroup <- newFocusGroup
    focusGroup `onKeyPressed` \_ key _ ->
        if key == KEsc || key == KASCII 'q' then exitSuccess else return False
    addToFocusGroup focusGroup menuSwitcher

    -- Wrap the widget to be shown in a collection and start the shit.
    collection <- newCollection
    _ <- addToCollection collection tableWidget focusGroup
    runUi collection defaultContext{skin=unicodeRoundedSkin}



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
    
    