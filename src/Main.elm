module Main exposing (..)

import Browser
import Html exposing (Html, section)
import Html.Attributes exposing (class)
import SuperTicTacToe exposing (GamePosition, SuperTicTacToe)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    SuperTicTacToe


init : Model
init =
    SuperTicTacToe.empty


type Msg
    = GameMove GamePosition


update : Msg -> Model -> Model
update msg model =
    case msg of
        GameMove gamePos ->
            SuperTicTacToe.update gamePos model


view : Model -> Html Msg
view model =
    section [ class "app" ]
        [ SuperTicTacToe.viewBoard model GameMove
        ]
