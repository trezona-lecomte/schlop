{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Applicative                  (liftA2, liftA3)
import           Data.Aeson                           (FromJSON, ToJSON)
import qualified Data.Aeson                           as Aeson
import           Data.CaseInsensitive                 (CI)
import qualified Data.CaseInsensitive                 as CI
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           GHC.Generics                         (Generic)
import           Servant                              (FromHttpApiData)


-- Domain Types

newtype ProtoUser = ProtoUser
  { email :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProtoUser
instance FromJSON ProtoUser

data User = User
  { id    :: UserId
  , email :: CI Text
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

instance FromRow User where
  fromRow = liftA2 User field field

newtype UserId = UserId Integer
  deriving (Eq, Show, Num, Generic, ToField, FromField)

instance ToJSON UserId
instance FromJSON UserId


data ProtoShoppingList = ProtoShoppingList
  { name      :: Text
  , creatorId :: UserId
  } deriving (Eq, Show, Generic)

instance ToJSON ProtoShoppingList
instance FromJSON ProtoShoppingList

data ShoppingList = ShoppingList
  { id        :: ShoppingListId
  , name      :: Text
  , creatorId :: UserId
  } deriving (Eq, Show, Generic)

instance FromRow ShoppingList where
  fromRow = liftA3 ShoppingList field field field

instance ToJSON ShoppingList
instance FromJSON ShoppingList

newtype ShoppingListId = ShoppingListId Integer
  deriving (Eq, Show, Num, Generic, ToField, FromField, FromHttpApiData)

instance ToJSON ShoppingListId
instance FromJSON ShoppingListId


data ProtoItem = ProtoItem
  { description    :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProtoItem
instance FromJSON ProtoItem

data Item = Item
  { id             :: ItemId
  , description    :: Text
  , shoppingListId :: ShoppingListId
  } deriving (Eq, Show, Generic)

instance FromRow Item where
  fromRow = liftA3 Item field field field

instance ToJSON Item
instance FromJSON Item

newtype ItemId = ItemId Integer
  deriving (Eq, Show, Num, Generic, ToField, FromField)

instance ToJSON ItemId
instance FromJSON ItemId


-- Case Insensitivity

instance FromJSON (CI Text) where
  parseJSON (Aeson.String a) = pure $ CI.mk a
  parseJSON value            = fail $ "Expected String, got: " ++ show value

instance ToJSON (CI Text) where
  toJSON a = Aeson.String (CI.original a)
