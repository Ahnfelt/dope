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
    = OK PlayerIntrospection [Option]
    | Error Error
    | Bye
    deriving (Show, Read, Eq)

data Error
    = InvalidRequest
    | IllegalAct String PlayerIntrospection [Option]
    | PlayerDoesNotExist
    | NotLoggedIn
    | PlayerAlreadyExists
    deriving (Show, Read, Eq)
    
data Client = Client (Request -> IO (Either Response (Response, Client)))

