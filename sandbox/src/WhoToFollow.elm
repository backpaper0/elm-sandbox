module WhoToFollow exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Http
import Json.Decode as D


---- MODEL ----


type alias User =
    { avatarUrl : String, htmlUrl : String, login : String }


type alias Row =
    { user : Maybe User, users : List User }


type alias Model =
    List ( Int, Row )


init : () -> ( Model, Cmd Msg )
init _ =
    ( List.repeat 3 (Row (Just (User "" "" "foo")) [ User "" "" "foo", User "" "" "bar", User "" "" "baz" ]) |> List.indexedMap Tuple.pair, Cmd.none )



---- UPDATE ----


type Msg
    = NextUser ( Int, Row )
    | DecidedUser Int (Maybe User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextUser ( index, row ) ->
            let
                f cond a ( i, b ) =
                    ( i + 1
                    , if (i == cond) then
                        Just a
                      else
                        b
                    )

                getUser a =
                    row.users |> List.foldl (f a) ( 1, Nothing ) |> Tuple.second

                generator =
                    Random.int 1 (List.length row.users) |> Random.map getUser
            in
                ( model, Random.generate (DecidedUser index) generator )

        DecidedUser index user ->
            let
                f ( i, r ) =
                    ( i
                    , if (i == index) then
                        { r | user = user }
                      else
                        r
                    )
            in
                ( model |> List.map f, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        css =
            { container = [ style "font-family" "sans-serif", style "padding" "10px" ]
            , header = [ style "background" "#ececec", style "padding" "5px" ]
            , title = [ style "font-weight" "bold", style "display" "inline-block" ]
            , refresh = [ style "font-size" "80%", style "margin-left" "10px" ]
            , suggestions = [ style "border" "2px solid #ececec", style "list-style" "none" ]
            , suggestion = [ style "padding" "5px" ]
            , avatar = [ style "width" "40px", style "height" "40px", style "border-radius" "20px" ]
            , username = [ style "display" "inline-block", style "position" "relative", style "bottom" "15px", style "left" "5px" ]
            , close = [ style "margin-left" "10px" ]
            }

        userHtml ( index, row ) =
            case row.user of
                Just user ->
                    li css.suggestion
                        [ img (css.avatar ++ [ src user.avatarUrl ]) []
                        , a (css.username ++ [ href user.htmlUrl, target "_blank" ]) [ text user.login ]
                        , a (css.username ++ css.close ++ [ href "#", onClick (NextUser ( index, row )) ]) [ text "x" ]
                        ]

                Nothing ->
                    li css.suggestion
                        [ img (css.avatar ++ []) []
                        , a (css.username ++ [ href "#", target "_blank" ]) []
                        , a (css.username ++ css.close ++ [ href "#" ]) [ text "x" ]
                        ]
    in
        div css.container
            [ div css.header
                [ h2 css.title [ text "Who to follow" ]
                , a (css.refresh ++ [ href "#" ]) [ text "Refresh" ]
                ]
            , ul css.suggestions (model |> List.map userHtml)
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
