{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module DB where

import           Control.Monad.IO.Class          (liftIO)
import           Data.Int                        (Int64)
import           Data.Pool                       (Pool, createPool,
                                                  withResource)
import qualified Data.Pool                       as Pool
import           Database.PostgreSQL.Simple      (Connection, close,
                                                  connectPostgreSQL)
import           Database.PostgreSQL.Transaction (PGTransaction, query, query_,
                                                  runPGTransactionIO)
import           Types

runDB :: Pool Connection -> PGTransaction a -> IO a
runDB connectionPool action = withResource connectionPool $ \connection ->
  runPGTransactionIO action connection

selectUsers :: PGTransaction [User]
selectUsers = query_ "select id, email from users"

insertUser :: ProtoUser -> PGTransaction User
insertUser ProtoUser{email} = do
  [user] :: [User] <- query [email] "insert into users (email) values (?) returning id, email"
  return user

selectShoppingLists :: PGTransaction [ShoppingList]
selectShoppingLists = query_
  "select id, name, creator_id from shopping_lists"

insertShoppingList :: ProtoShoppingList -> PGTransaction ShoppingList
insertShoppingList ProtoShoppingList{..} = do
  [shoppingList] :: [ShoppingList] <-
    query (name, creatorId)
     "insert into shopping_lists (name, creator_id) values (?, ?) returning id, name, creator_id"
  return shoppingList

selectItems :: Int64 -> PGTransaction [Item]
selectItems listId = query [listId]
  "select id, description, shopping_list_id from items where shopping_list_id = ?"

insertItem :: Int64 -> ProtoItem -> PGTransaction Item
insertItem shoppingListId ProtoItem{description} = do
  [user] :: [Item] <-
    query (description, shoppingListId)
      "insert into items (description, shopping_list_id) values (?, ?) returning id, description, shopping_list_id"
  return user

