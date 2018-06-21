module Generated.SchlopApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


import Generated.Types exposing (..)

import Json.Decode exposing (..)

import Json.Decode.Pipeline exposing (..)

getUsers : Http.Request (List (User))
getUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postUsers : ProtoUser -> Http.Request (User)
postUsers body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "users"
                ]
        , body =
            Http.jsonBody (encodeProtoUser body)
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getShopping_lists : Http.Request (List (ShoppingList))
getShopping_lists =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "shopping_lists"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeShoppingList)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postShopping_lists : ProtoShoppingList -> Http.Request (ShoppingList)
postShopping_lists body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "shopping_lists"
                ]
        , body =
            Http.jsonBody (encodeProtoShoppingList body)
        , expect =
            Http.expectJson decodeShoppingList
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getShopping_listsByShopping_list_idItems : Int -> Http.Request (List (Item))
getShopping_listsByShopping_list_idItems capture_shopping_list_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "shopping_lists"
                , capture_shopping_list_id |> toString |> Http.encodeUri
                , "items"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeItem)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postShopping_listsByShopping_list_idItems : Int -> ProtoItem -> Http.Request (Item)
postShopping_listsByShopping_list_idItems capture_shopping_list_id body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "shopping_lists"
                , capture_shopping_list_id |> toString |> Http.encodeUri
                , "items"
                ]
        , body =
            Http.jsonBody (encodeProtoItem body)
        , expect =
            Http.expectJson decodeItem
        , timeout =
            Nothing
        , withCredentials =
            False
        }

encodeProtoUser : ProtoUser -> Json.Encode.Value
encodeProtoUser x =
    Json.Encode.object
        [ ( "email", Json.Encode.string x.email )
        ]

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "email", Json.Encode.string x.email )
        ]

encodeProtoShoppingList : ProtoShoppingList -> Json.Encode.Value
encodeProtoShoppingList x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "creatorId", Json.Encode.int x.creatorId )
        ]

encodeShoppingList : ShoppingList -> Json.Encode.Value
encodeShoppingList x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "creatorId", Json.Encode.int x.creatorId )
        ]

encodeProtoItem : ProtoItem -> Json.Encode.Value
encodeProtoItem x =
    Json.Encode.object
        [ ( "description", Json.Encode.string x.description )
        ]

encodeItem : Item -> Json.Encode.Value
encodeItem x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "description", Json.Encode.string x.description )
        , ( "shoppingListId", Json.Encode.int x.shoppingListId )
        ]

decodeProtoUser : Decoder ProtoUser
decodeProtoUser =
    decode ProtoUser
        |> required "email" string

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "id" int
        |> required "email" string

decodeProtoShoppingList : Decoder ProtoShoppingList
decodeProtoShoppingList =
    decode ProtoShoppingList
        |> required "name" string
        |> required "creatorId" int

decodeShoppingList : Decoder ShoppingList
decodeShoppingList =
    decode ShoppingList
        |> required "id" int
        |> required "name" string
        |> required "creatorId" int

decodeProtoItem : Decoder ProtoItem
decodeProtoItem =
    decode ProtoItem
        |> required "description" string

decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "id" int
        |> required "description" string
        |> required "shoppingListId" int