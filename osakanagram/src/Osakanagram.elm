module Osakanagram exposing (main)

import Browser
import Json.Decode as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


type alias Photo =
    { title : String, image : String, fav : Int }


type alias Model =
    List Photo


type Msg
    = ClickStar Int


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init () =
    let
        model =
            [ { title = "肉重", image = "http://localhost:8080/images/54277395_270952053826359_6864398686538064173_n.jpg", fav = 1 }
            , { title = "カレー", image = "http://localhost:8080/images/51449439_382792795837276_3157664589089934568_n.jpg", fav = 0 }
            ]
    in
        ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ header
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "40px"
            , style "borderBottom" "1px solid silver"
            , style "boxSizing" "border-box"
            , style "display" "flex"
            , style "alignItems" "center"
            , style "backgroundColor" "white"
            ]
            [ h1
                [ style "fontSize" "large"
                , style "fontStyle" "italic"
                , style "margin" "0"
                ]
                [ text "Osakanagram" ]
            ]
        , div
            [ style "width" "500px"
            , style "margin" "65px auto"
            ]
          <|
            List.indexedMap photoView model
        ]


photoView : Int -> Photo -> Html Msg
photoView index { title, image, fav } =
    div
        [ style "border" "1px solid silver"
        , style "margin" "25px 0"
        ]
        [ div [] [ img [ style "width" "100%", src image ] [] ]
        , div [ style "padding" "0 5px" ] [ text title ]
        , div [ style "padding" "0 5px" ]
            [ button
                [ style "cursor" "pointer"
                , style "border" "0"
                , style "fontSize" "large"
                , style "backgroundColor" "transparent"
                , style "color" <|
                    if fav == 0 then
                        "gray"
                    else
                        "PaleVioletRed"
                , onClick <| ClickStar index
                ]
                [ text <|
                    if fav == 0 then
                        "☆"
                    else
                        "★"
                ]
            , span [] [ text "いいね！", span [] [ text <| String.fromInt fav ], text "件" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickStar index ->
            let
                favStar i photo =
                    if i == index then
                        { photo | fav = photo.fav + 1 }
                    else
                        photo

                newModel =
                    List.indexedMap favStar model
            in
                ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
