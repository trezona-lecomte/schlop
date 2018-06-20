{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    ) where

import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Types

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "foo@bar.com"
        , User 2 "baz@qux.com"
        ]
