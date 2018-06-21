{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Documentation (markdownDoc) where

import           Lib          (api)
import           Servant      (Capture)
import           Servant.Docs (DocCapture (..), ToCapture, ToSample,
                               docsWithIntros, markdown, singleSample,
                               toCapture, toSamples)
import           Types

markdownDoc :: String
markdownDoc = markdown $ docsWithIntros [] api

instance ToSample ProtoUser where
  toSamples _ =
    singleSample (ProtoUser "foo@bar.com")

instance ToSample User where
  toSamples _ = singleSample (User 1 "foo@bar.com")

instance ToSample ProtoShoppingList where
  toSamples _ =
    singleSample (ProtoShoppingList "My Shopping List" 1)

instance ToSample ShoppingList where
  toSamples _ = singleSample (ShoppingList 2 "My Shopping List" 1)

instance ToSample ProtoItem where
  toSamples _ =
    singleSample (ProtoItem "Bread")

instance ToSample Item where
  toSamples _ = singleSample (Item 3 "Bread" 2)

instance ToCapture (Capture "shopping_list_id" ShoppingListId) where
  toCapture _ =
    DocCapture "shopping_list_id" "(integer) id of the shopping list to fetch items for"


