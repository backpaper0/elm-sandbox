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
        css =
            { container = [ style "font-family" "sans-serif", style "padding" "10px" ]
            , header = [ style "background" "#ececec", style "padding" "5px" ]
            , title = [ style "font-weight" "bold", style "display" "inline-block" ]
            , refresh = [ style "font-size" "80%", style "margin-left" "10px" ]
            , suggestions = [ style "border" "2px solid #ececec", style "list-style" "none" ]
            , avatar = [ style "width" "40px", style "height" "40px", style "border-radius" "20px" ]
            , username = [ style "display" "inline-block", style "position" "relative", style "bottom" "15px", style "left" "5px" ]
            , close = [ style "margin-left" "10px" ]
            }

        userHtml user =
            li [ style "padding" "5px" ]
                [ img (css.avatar ++ [ src user.avatarUrl ]) []
                , a (css.username ++ [ href user.htmlUrl, target "_blank" ]) [ text user.login ]
                , a (css.username ++ css.close ++ [ href "#" ]) [ text "x" ]
                ]
    in
        div css.container
            [ div css.header
                [ h2 css.title [ text "Who to follow" ]
                , a (css.refresh ++ [ href "#" ]) [ text "Refresh" ]
                ]
            , ul css.suggestions (users |> List.map userHtml)
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
