module WhoToFollow exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Http
import Json.Decode as D
import Task


---- MODEL ----


type alias User =
    { avatarUrl : String, htmlUrl : String, login : String }


type alias Row =
    { user : Maybe User, users : List User }


type alias Model =
    List ( Int, Row )


updateRow index mapper model =
    let
        f ( i, row ) =
            ( i
            , if (i == index) then
                mapper row
              else
                row
            )
    in
        model |> List.map f


init : () -> ( Model, Cmd Msg )
init _ =
    let
        f () =
            Refresh
    in
        ( List.repeat 3 (Row Nothing []) |> List.indexedMap Tuple.pair, Task.succeed () |> Task.perform f )



---- UPDATE ----


type Msg
    = Refresh
    | DecidedOffset Int Int
    | GotUsers Int (Result Http.Error (List User))
    | NextUser ( Int, List User )
    | DecidedUser Int (Maybe User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            let
                nextOffset index =
                    Random.generate (DecidedOffset index) (Random.int 0 499)

                nextOffsets =
                    model |> List.map Tuple.first |> List.map nextOffset |> Cmd.batch
            in
                ( model, nextOffsets )

        DecidedOffset index offset ->
            let
                decoder =
                    D.list <|
                        D.map3
                            User
                            (D.field "avatar_url" D.string)
                            (D.field "html_url" D.string)
                            (D.field "login" D.string)

                getUsers =
                    Http.get
                        { url = "https://api.github.com/users?since=" ++ (String.fromInt offset)
                        , expect = Http.expectJson (GotUsers index) decoder
                        }
            in
                ( model, getUsers )

        GotUsers index (Ok users) ->
            let
                setUsers r =
                    { r | users = users }

                nextUser =
                    Task.succeed ( index, users ) |> Task.perform NextUser
            in
                ( updateRow index setUsers model, nextUser )

        GotUsers _ (Err _) ->
            ( model, Cmd.none )

        NextUser ( index, users ) ->
            let
                f cond a ( i, b ) =
                    ( i + 1
                    , if (i == cond) then
                        Just a
                      else
                        b
                    )

                getUser a =
                    users |> List.foldl (f a) ( 1, Nothing ) |> Tuple.second

                generator =
                    Random.int 1 (List.length users) |> Random.map getUser
            in
                ( model, Random.generate (DecidedUser index) generator )

        DecidedUser index user ->
            let
                setUser r =
                    { r | user = user }
            in
                ( updateRow index setUser model, Cmd.none )



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
                        , a (css.username ++ css.close ++ [ href "#", onClick (NextUser ( index, row.users )) ]) [ text "x" ]
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
                , a (css.refresh ++ [ href "#", onClick Refresh ]) [ text "Refresh" ]
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
