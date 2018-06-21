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
server p = getUsers p
  :<|> createUser p
  :<|> getShoppingLists p
  :<|> createShoppingList p
  :<|> getItems p
  :<|> createItem p


server' :: Pool Connection -> Server API'
server' connectionPool = server connectionPool
  :<|> serveDirectoryFileServer "elm"

app :: Pool Connection -> Application
app connectionPool = serve api' $ server' connectionPool


-- Endpoint Handlers


getUsers :: Pool Connection -> Handler [User]
getUsers pool =
  liftIO . withResource pool $ \conn ->
    query_ conn "select id, email from users"

createUser :: Pool Connection -> ProtoUser -> Handler User
createUser pool proto =
  liftIO . withResource pool $ \conn -> do
    [user] :: [User] <-
      query conn "insert into users (email) values (?) returning id, email" [email (proto :: ProtoUser)]
    return user

getShoppingLists :: Pool Connection -> Handler [ShoppingList]
getShoppingLists pool =
  liftIO . withResource pool $ \conn ->
    query_ conn "select id, name, creator_id from shopping_lists"

createShoppingList :: Pool Connection -> ProtoShoppingList -> Handler ShoppingList
createShoppingList pool ProtoShoppingList{..} =
  liftIO . withResource pool $ \conn -> do
    [shoppingList] :: [ShoppingList] <-
      query conn "insert into shopping_lists (name, creator_id) values (?, ?) returning id, name, creator_id" (name, creatorId)
    return shoppingList

getItems :: Pool Connection -> Int64 -> Handler [Item]
getItems pool listId =
  liftIO . withResource pool $ \conn ->
    query conn "select id, description, shopping_list_id from items where shopping_list_id = ?" [listId]

createItem :: Pool Connection -> Int64 -> ProtoItem -> Handler Item
createItem pool shoppingListId ProtoItem{..} =
  liftIO . withResource pool $ \conn -> do
    [user] :: [Item] <-
      query conn "insert into items (description, shopping_list_id) values (?, ?) returning id, description, shopping_list_id" (description, shoppingListId)
    return user


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
