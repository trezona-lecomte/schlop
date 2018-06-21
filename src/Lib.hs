{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Lib (startApp, api) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import           Data.Int                   (Int64)
import           Database.PostgreSQL.Simple (connectPostgreSQL, query, query_)
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant
-- import qualified Servant.Utils.StaticFiles  as Static
import           Types


type API = "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] ProtoUser :> Post '[JSON] User
  :<|> "shopping_lists" :> Get '[JSON] [ShoppingList]
  :<|> "shopping_lists" :> ReqBody '[JSON] ProtoShoppingList :> Post '[JSON] ShoppingList
  :<|> "shopping_lists" :> Capture "shopping_list_id" Int64 :> "items" :> Get '[JSON] [Item]
  :<|> "shopping_lists" :> Capture "shopping_list_id" Int64 :> "items" :> ReqBody '[JSON] ProtoItem :> Post '[JSON] Item

type API' = API :<|> Raw

startApp :: IO ()
startApp = Warp.run 8000 app

app :: Application
app = serve api' server'

api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: Server API
server = getUsers
  :<|> createUser
  :<|> getShoppingLists
  :<|> createShoppingList
  :<|> getItems
  :<|> createItem


server' :: Server API'
server' = server
  :<|> serveDirectoryFileServer "elm"

-- Endpoint Handlers

getUsers :: Handler [User]
getUsers = do
  conn <- liftIO $ connectPostgreSQL libpqConnString
  liftIO $ query_ conn "select id, email from users"

createUser :: ProtoUser -> Handler User
createUser proto = do
  conn <- liftIO $ connectPostgreSQL libpqConnString
  [user] :: [User] <- liftIO $
    query conn "insert into users (email) values (?) returning id, email" [email (proto :: ProtoUser)]
  return user

getShoppingLists :: Handler [ShoppingList]
getShoppingLists = do
  conn <- liftIO $ connectPostgreSQL libpqConnString
  liftIO $ query_ conn "select id, name, creator_id from shopping_lists"

createShoppingList :: ProtoShoppingList -> Handler ShoppingList
createShoppingList proto = do
  conn <- liftIO $ connectPostgreSQL libpqConnString
  [shoppingList] :: [ShoppingList] <- liftIO $
    query
      conn
      "insert into shopping_lists (name, creator_id) values (?, ?) returning id, name, creator_id"
      (name (proto :: ProtoShoppingList), creatorId (proto :: ProtoShoppingList))
  return shoppingList

getItems :: Int64 -> Handler [Item]
getItems listId = do
  conn <- liftIO $ connectPostgreSQL libpqConnString
  liftIO $ query conn "select id, description, shopping_list_id from items where shopping_list_id = ?" [listId]

createItem :: Int64 -> ProtoItem -> Handler Item
createItem shoppingListId proto = do
  conn <- liftIO $ connectPostgreSQL libpqConnString
  [user] :: [Item] <- liftIO $
    query
      conn
      "insert into items (description, shopping_list_id) values (?, ?) returning id, description, shopping_list_id"
      (description (proto :: ProtoItem), shoppingListId)
  return user


-- Config

libpqConnString =
  "host=localhost port=5432 dbname=schlop"
