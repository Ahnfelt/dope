import Control.Monad
import System.Random

data State
    = Jail
    | Street
    | Club
    | Busted State

type Action = String

type M = IO

goto :: State -> [(Action, M State)]
goto state = case state of
    Jail -> [
        ("Bail yourself out", return Street)
        ]
    Street -> [
        ("Sell drugs", do
            return Street
        ),
        ("Go to the club", ifNotBusted state $ do
            return Club
        )
        ]
    Club -> [
        ("Go to the street", ifNotBusted state $ do
            return Street
        )
        ]
    Busted location -> [
        ("Go to jail", return Jail),
        ("Bribe the policeman", do
            r <- randomIO :: IO Double
            if r < 0.5 
                then return Jail
                else return location
        ),
        ("Snitch on a friend", do
            r <- randomIO :: IO Double
            if r < 0.5
                then return Jail
                else return location
        )
        ]

describe state = case state of
    Jail -> "You're in jail."
    Street -> "You're at the streets."
    Club -> "You're in a club."
    Busted _ -> "You've got busted!!"

ifNotBusted :: State -> M State -> M State
ifNotBusted location action = do
    r <- randomIO :: IO Double
    if r < 0.2
        then return (Busted location)
        else action
        
main = loop Jail

loop :: State -> M ()
loop state = do
    let options = goto state
    state' <- askUserToPickAnOptionFrom state options
    loop state'

askUserToPickAnOptionFrom :: State -> [(Action, M State)] -> M State
askUserToPickAnOptionFrom state options = do
    putStrLn (describe state)
    let menu = zip [1, 2 ..] (map fst options)
    forM menu $ \(n, s) -> putStrLn ("(" ++ (show n) ++ ") " ++ s)
    ask
    where 
        ask = do
            input <- getLine
            let choice = read input :: Int
            if choice < 1 || choice > length options
                then do
                    putStrLn "Very funny."
                    ask
                else do
                    putStrLn ""
                    snd (options !! (choice - 1))

