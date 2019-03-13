module SpaExample.Timer exposing (..)

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


type Msg
    = Start
    | Stop
    | Tick


type alias Model =
    { value : Int, active : Bool }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model 0 False, Cmd.none )


view : Model -> Html Msg
view { value, active } =
    div []
        [ h1 [] [ text "Timer" ]
        , p [ style "font-size" "xx-large" ] [ text <| String.fromInt value ]
        , button
            [ onClick <|
                if active then
                    Stop
                else
                    Start
            ]
            [ text <|
                if active then
                    "Stop"
                else
                    "Start"
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | active = True }, Cmd.none )

        Stop ->
            ( { model | active = False }, Cmd.none )

        Tick ->
            ( { model | value = model.value + 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { active } =
    if active then
        (Time.every 100 (\_ -> Tick))
    else
        Sub.none
