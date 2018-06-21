module Schlop exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Generated.Types exposing (..)
import Generated.SchlopApi exposing (..)
import Http


-- MODEL


type alias Model =
    { user : Maybe User
    , shoppingLists : List ShoppingList
    , items : List Item
    , error : String
    }


init : ( Model, Cmd Msg )
init =
    { user = Nothing
    , shoppingLists = []
    , items = []
    , error = ""
    }
        ! [ Http.send LoadShoppingLists getShopping_lists
          ]



-- UPDATE


type Msg
    = CreateShoppingList
    | UpdateShoppingListTitle ShoppingList String
    | CreateItem ShoppingList
    | UpdateItemDescription Item String
    | LoadShoppingLists (Result Http.Error (List ShoppingList))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateShoppingList ->
            ( model, Cmd.none )

        UpdateShoppingListTitle shoppingList newTitle ->
            ( model, Cmd.none )

        CreateItem shoppingList ->
            ( model, Cmd.none )

        UpdateItemDescription item newDescription ->
            ( model, Cmd.none )

        LoadShoppingLists result ->
            case result of
                Err e ->
                    ( { model | error = toString e }, Cmd.none )

                Ok loadedShoppingLists ->
                    ( { model | shoppingLists = loadedShoppingLists }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewTitle
        , section [ class "section" ]
            [ div [ class "container" ]
                [ viewCreateListButton
                , viewShoppingLists model
                ]
            ]
        , section [ class "section" ]
            [ p [] [ text model.error ] ]
        ]


viewTitle : Html Msg
viewTitle =
    section [ class "hero is-primary" ]
        [ div [ class "hero-body" ] [ viewHeader ] ]


viewHeader : Html Msg
viewHeader =
    div [ class "container" ]
        [ h1 [ class "title" ]
            [ text "Welcome to Schlop" ]
        , h2 [ class "subtitle" ]
            [ text "Time to get schloppy." ]
        ]


viewCreateListButton : Html Msg
viewCreateListButton =
    div [ class "section is-horizontally-centered" ]
        [ a
            [ class "button is-primary is-outlined"
            , onClick CreateShoppingList
            ]
            [ span []
                [ text "New list" ]
            , span [ class "icon is-small" ]
                [ i [ class "fa fa-cart-plus" ] [] ]
            ]
        ]


viewShoppingLists : Model -> Html Msg
viewShoppingLists model =
    section [ class "section" ]
        [ div [ class "columns is-variable is-8 is-centered is-multiline" ] <|
            List.map (viewShoppingList model) model.shoppingLists
        ]


viewShoppingList : Model -> ShoppingList -> Html Msg
viewShoppingList model shoppingList =
    div [ class "column is-half" ]
        [ div [ class "columns is-mobile is-vertically-centered" ]
            [ div [ class "column" ]
                [ viewShoppingListTitle shoppingList ]
            , div [ class "column is-narrow" ]
                [-- viewDeleteListButton shoppingList
                ]
            ]
        , viewItemsInShoppingList model shoppingList
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ viewCreateItemButton shoppingList ]
            , div [ class "column has-gap-below" ]
                [-- clearCheckedItemsButton list
                ]
            ]
        ]


viewShoppingListTitle : ShoppingList -> Html Msg
viewShoppingListTitle shoppingList =
    input
        [ class "input"
        , placeholder "List title"
        , UpdateShoppingListTitle shoppingList |> onInput
        , value shoppingList.name
        ]
        []


viewItemsInShoppingList : Model -> ShoppingList -> Html Msg
viewItemsInShoppingList model shoppingList =
    div [ class "box is-borderless" ] <|
        List.map viewItem (List.filter (\i -> i.shoppingListId == shoppingList.id) model.items)


viewItem : Item -> Html Msg
viewItem item =
    div [ class "columns is-mobile is-vertically-centered" ]
        [ input
            [ class "column input is-borderless"
            , Html.Attributes.id <| toString <| item.id
            , placeholder "Item name"
            , value item.description
            , UpdateItemDescription item |> onInput
            ]
            []
        , label
            [ class "column is-narrow checkbox" ]
            [ input
                [ type_ "checkbox"

                -- , checked item.completed
                -- , CheckItem item |> onCheck
                ]
                []
            ]
        , button
            [ class "column is-narrow button delete"

            -- , DeleteItem item |> onClick
            ]
            []
        ]


viewCreateItemButton : ShoppingList -> Html Msg
viewCreateItemButton shoppingList =
    button
        [ class "button"
        , CreateItem shoppingList |> onClick
        ]
        [ text "Add item"
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
