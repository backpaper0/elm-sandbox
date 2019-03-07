module TickExample exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { posix : String }


init () =
    ( { posix = "0" }, Cmd.none )


type Msg
    = Tick Time.Posix


update msg model =
    case msg of
        Tick posix ->
            ( { model | posix = Time.posixToMillis posix |> String.fromInt }, Cmd.none )


view model =
    div [ style "margin" "2rem" ]
        [ text model.posix ]


subscriptions model =
    Time.every 100 Tick
