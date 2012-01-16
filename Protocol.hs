module Protocol where

import Dope
import State
import ClientData

data Request
    = UsePlayer String
    | NewPlayer String
    | Act Option
    | Quit
    deriving (Show, Read, Eq)
    
data Response
    = OK Player [Option]
    | Error Error
    | Bye
    deriving (Show, Read, Eq)

data Error
    = InvalidRequest
    | IlligalAct String Player [Option]
    | PlayerDoesNotExist
    | NotLoggedIn
    | PlayerAlreadyExist
    deriving (Show, Read, Eq)
    
data Client = Client (Request -> IO (Either Response (Response, Client)))
