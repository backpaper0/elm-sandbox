module WhoToFollow exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


---- MODEL ----


type alias User =
    { avatarUrl : String, htmlUrl : String, login : String }


type alias Model =
    { users : List User }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { users = List.repeat 3 (User "https://avatars1.githubusercontent.com/u/209262?v=4" "https://github.com/backpaper0" "backpaper0") }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { users } =
    let
        userHtml user =
            li []
                [ img [ src user.avatarUrl ] []
                , a [ class "username", href user.htmlUrl, target "_blank" ] [ text user.login ]
                , text " "
                , a [ class "close", href "#" ] [ text "x" ]
                ]
    in
        div [ class "container" ]
            [ div [ class "header" ]
                [ h2 [] [ text "Who to follow" ]
                , text " "
                , a [ href "#" ] [ text "Refresh" ]
                ]
            , ul [ class "suggestions" ] (users |> List.map userHtml)
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
