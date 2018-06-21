{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Lib (startApp, api) where

import           Control.Concurrent
import           Control.Exception          (bracket)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Pool                  (Pool, createPool, withResource)
import qualified Data.Pool                  as Pool
import           Database.PostgreSQL.Simple (Connection, close,
                                             connectPostgreSQL, query, query_)
import qualified DB
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant
import           Servant.Client             (BaseUrl (BaseUrl), Scheme (Http),
                                             mkClientEnv, runClientM)
import           Types

type API = "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] ProtoUser :> Post '[JSON] User
  :<|> "shopping_lists" :> Get '[JSON] [ShoppingList]
  :<|> "shopping_lists" :> ReqBody '[JSON] ProtoShoppingList :> Post '[JSON] ShoppingList
  :<|> "shopping_lists" :> Capture "shopping_list_id" Int64 :> "items" :> Get '[JSON] [Item]
  :<|> "shopping_lists" :> Capture "shopping_list_id" Int64 :> "items" :> ReqBody '[JSON] ProtoItem :> Post '[JSON] Item

type API' = API :<|> Raw

api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: Pool Connection -> Server API
server p = query0 DB.selectUsers
  :<|> query1 DB.insertUser
  :<|> query0 DB.selectShoppingLists
  :<|> query1 DB.insertShoppingList
  :<|> query1 DB.selectItems
  :<|> query2 DB.insertItem
  where
    -- Oh god, what have I done...
    query0 q = liftIO $ DB.runDB p q
    query1 q a = liftIO $ DB.runDB p (q a)
    query2 q a b = liftIO $ DB.runDB p (q a b)



server' :: Pool Connection -> Server API'
server' connectionPool = server connectionPool
  :<|> serveDirectoryFileServer "elm"

app :: Pool Connection -> Application
app connectionPool = serve api' $ server' connectionPool


-- Wiring

runApp :: Pool Connection -> IO ()
runApp connectionPool = Warp.run 8000 $ app connectionPool

startApp :: IO ()
startApp = do
  -- We should read the connection string from a config file,
  -- environment variable, or somewhere else instead.
  pool <- initConnectionPool libpqConnString
  runApp pool


-- DB

libpqConnString =
  "host=localhost port=5432 dbname=schlop"

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe
