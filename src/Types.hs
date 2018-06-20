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
import           GHC.Generics                         (Generic)


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
  deriving (Eq, Show, Num, Generic, FromField)

instance ToJSON UserId
instance FromJSON UserId


data ShoppingList = ShoppingList
  { id      :: Integer
  , name    :: Text
  , creator :: Integer
  } deriving (Eq, Show, Generic)

instance FromRow ShoppingList where
  fromRow = liftA3 ShoppingList field field field

instance ToJSON ShoppingList
instance FromJSON ShoppingList

newtype ShoppingListId = ShoppingListId Integer
  deriving (Eq, Show, Num, Generic, FromField)

instance ToJSON ShoppingListId
instance FromJSON ShoppingListId


data Item = Item
  { id          :: Integer
  , listId      :: Integer
  , description :: Text
  } deriving (Eq, Show, Generic)

instance FromRow Item where
  fromRow = liftA3 Item field field field

instance ToJSON Item
instance FromJSON Item

newtype ItemId = ItemId Integer
  deriving (Eq, Show, Num, Generic, FromField)

instance ToJSON ItemId
instance FromJSON ItemId


-- Case Insensitivity

instance FromJSON (CI Text) where
  parseJSON (Aeson.String a) = pure $ CI.mk a
  parseJSON value            = fail $ "Expected String, got: " ++ show value

instance ToJSON (CI Text) where
  toJSON a = Aeson.String (CI.original a)
