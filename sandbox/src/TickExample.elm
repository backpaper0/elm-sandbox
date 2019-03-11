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
    { posix : String, tick : Float }


init : () -> ( Model, Cmd Msg )
init () =
    ( { posix = "0", tick = 100 }, Cmd.none )


type Msg
    = Tick Time.Posix
    | Input (Maybe Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( { model | posix = Time.posixToMillis posix |> String.fromInt }, Cmd.none )

        Input (Just tick) ->
            ( { model | tick = tick }, Cmd.none )

        Input Nothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { posix, tick } =
    div []
        [ p [] [ text posix ]
        , p [] [ input [ value <| String.fromFloat tick, onInput <| String.toFloat >> Input ] [] ]
        ]


subscriptions : Model -> Sub Msg
subscriptions { tick } =
    Time.every tick Tick
