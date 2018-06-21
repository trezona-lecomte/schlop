{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ClientGenerator where

import           Data.Text      as T (Text)
import qualified Data.Text      as T
import           Data.Text.IO   as T (readFile, writeFile)
import           Elm            (toElmDecoderSource, toElmEncoderSource,
                                 toElmTypeSource)
import           GHC.Generics
import           Lib            (api)
import           Servant
import qualified Servant.Elm    as Elm
import qualified Servant.Kotlin as Kotlin
import           Types


-- Elm Client

typeSpec :: Elm.Spec
typeSpec = Elm.Spec ["Generated", "Types"]
    [ toElmTypeSource (Proxy :: Proxy ProtoUser)
    , toElmTypeSource (Proxy :: Proxy User)
    , toElmTypeSource (Proxy :: Proxy ProtoShoppingList)
    , toElmTypeSource (Proxy :: Proxy ShoppingList)
    , toElmTypeSource (Proxy :: Proxy ProtoItem)
    , toElmTypeSource (Proxy :: Proxy Item)
    ]


apiSpec :: Elm.Spec
apiSpec = Elm.Spec ["Generated", "SchlopApi"]
          (
            [ Elm.defElmImports
            , "import Generated.Types exposing (..)"
            , "import Json.Decode exposing (..)"
            , "import Json.Decode.Pipeline exposing (..)"
            ] ++ Elm.generateElmForAPI api ++
            [ toElmEncoderSource (Proxy :: Proxy ProtoUser)
            , toElmEncoderSource (Proxy :: Proxy User)
            , toElmEncoderSource (Proxy :: Proxy ProtoShoppingList)
            , toElmEncoderSource (Proxy :: Proxy ShoppingList)
            , toElmEncoderSource (Proxy :: Proxy ProtoItem)
            , toElmEncoderSource (Proxy :: Proxy Item)
            , toElmDecoderSource (Proxy :: Proxy ProtoUser)
            , toElmDecoderSource (Proxy :: Proxy User)
            , toElmDecoderSource (Proxy :: Proxy ProtoShoppingList)
            , toElmDecoderSource (Proxy :: Proxy ShoppingList)
            , toElmDecoderSource (Proxy :: Proxy ProtoItem)
            , toElmDecoderSource (Proxy :: Proxy Item)
            ]
          )

main :: IO ()
main = Elm.specsToDir [typeSpec, apiSpec] "elm/src"
