{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}

module Types where

import           Control.Applicative                  (liftA2, liftA3)
import           Data.Aeson                           (FromJSON, ToJSON)
import qualified Data.Aeson                           as Aeson
import           Data.CaseInsensitive                 (CI)
import qualified Data.CaseInsensitive                 as CI
import           Data.Int                             (Int64)
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           Elm                                  (ElmDatatype,
                                                       genericToElmDatatype,
                                                       toElmType)
import           GHC.Generics                         (Generic, from)
import           Servant                              (FromHttpApiData)
import           Servant.Elm                          (ElmType (..))


-- Domain Types

newtype ProtoUser = ProtoUser
  { email :: Text
  } deriving (Eq, Show, Generic, ElmType)

instance ToJSON ProtoUser
instance FromJSON ProtoUser

data User = User
  { id    :: Int64
  , email :: CI Text
  } deriving (Eq, Show, Generic, ElmType)

instance ToJSON User
instance FromJSON User

instance FromRow User where
  fromRow = liftA2 User field field


data ProtoShoppingList = ProtoShoppingList
  { name      :: Text
  , creatorId :: Int64
  } deriving (Eq, Show, Generic, ElmType)

instance ToJSON ProtoShoppingList
instance FromJSON ProtoShoppingList

data ShoppingList = ShoppingList
  { id        :: Int64
  , name      :: Text
  , creatorId :: Int64
  } deriving (Eq, Show, Generic, ElmType)

instance FromRow ShoppingList where
  fromRow = liftA3 ShoppingList field field field

instance ToJSON ShoppingList
instance FromJSON ShoppingList


newtype ProtoItem = ProtoItem
  { description    :: Text
  } deriving (Eq, Show, Generic, ElmType)

instance ToJSON ProtoItem
instance FromJSON ProtoItem

data Item = Item
  { id             :: Int64
  , description    :: Text
  , shoppingListId :: Int64
  } deriving (Eq, Show, Generic, ElmType)

instance FromRow Item where
  fromRow = liftA3 Item field field field

instance ToJSON Item
instance FromJSON Item


-- Case Insensitivity

instance FromJSON (CI Text) where
  parseJSON (Aeson.String a) = pure $ CI.mk a
  parseJSON value            = fail $ "Expected String, got: " ++ show value

instance ToJSON (CI Text) where
  toJSON a = Aeson.String (CI.original a)

instance ElmType (CI Text) where
  toElmType :: CI Text -> ElmDatatype
  toElmType ciText = toElmType $ CI.original ciText
