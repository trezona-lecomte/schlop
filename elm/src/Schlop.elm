module Schlop exposing (..)

import Html exposing (..)
import Generated.Types exposing (..)
import Generated.SchlopApi exposing (..)


-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    {} ! []



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
