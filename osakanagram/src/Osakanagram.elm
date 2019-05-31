module Osakanagram exposing (main)

import Browser
import Json.Decode as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


main =
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
            [ div
                [ style "border" "1px solid silver"
                , style "margin" "25px 0"
                ]
                [ div [] [ img [ style "width" "100%", src "http://localhost:8080/images/51449439_382792795837276_3157664589089934568_n.jpg" ] [] ]
                , div [ style "padding" "0 5px" ] [ text "カレー" ]
                , div [ style "padding" "0 5px" ]
                    [ button
                        [ style "cursor" "pointer"
                        , style "border" "0"
                        , style "fontSize" "large"
                        , style "backgroundColor" "transparent"
                        , style "color" "gray"
                        ]
                        [ text "☆" ]
                    , span [] [ text "いいね！", span [] [ text "0" ], text "件" ]
                    ]
                ]
            ]
        ]
