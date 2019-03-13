module SpaExample.Counter exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Increment


type alias Model =
    { count : Int }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model 0, Cmd.none )


view : Model -> Html Msg
view { count } =
    div []
        [ h1 [] [ text "Counter" ]
        , p [ style "font-size" "xx-large" ] [ text <| String.fromInt count ]
        , button [ onClick Increment ] [ text "Increment" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
