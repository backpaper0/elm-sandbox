module SpaExample exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url exposing (..)
import Debug
import SpaExample.Counter as Counter
import SpaExample.Timer as Timer


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type Msg
    = ChangeUrl Browser.UrlRequest
    | ChangedUrl Url
    | CounterMsg Counter.Msg
    | TimerMsg Timer.Msg


type Page
    = CounterPage
    | TimerPage


urlToPage url =
    case url.fragment of
        Just "counter" ->
            CounterPage

        Just "timer" ->
            TimerPage

        _ ->
            CounterPage


type alias Model =
    { key : Nav.Key, page : Page, counter : Counter.Model, timer : Timer.Model }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        ( model1, cmd1 ) =
            Counter.init () |> Tuple.mapSecond (\cmd -> cmd |> Cmd.map CounterMsg)

        ( model2, cmd2 ) =
            Timer.init () |> Tuple.mapSecond (\cmd -> cmd |> Cmd.map TimerMsg)
    in
        ( Model key (urlToPage url) model1 model2, Cmd.batch [ cmd1, cmd2 ] )


menu =
    ul []
        [ li [] [ a [ href "#counter" ] [ text "Counter" ] ]
        , li [] [ a [ href "#timer" ] [ text "Timer" ] ]
        , li [] [ a [ href "https://github.com" ] [ text "GitHub" ] ]
        ]


view : Model -> Browser.Document Msg
view model =
    case model.page of
        CounterPage ->
            { title = "Counter", body = [ menu, (Counter.view model.counter) |> Html.map CounterMsg ] }

        TimerPage ->
            { title = "Timer", body = [ menu, (Timer.view model.timer) |> Html.map TimerMsg ] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangeUrl (Browser.External url) ->
            ( model, Nav.load url )

        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )

        CounterMsg msg2 ->
            let
                ( counter, cmd ) =
                    Counter.update msg2 model.counter
            in
                ( { model | counter = counter }, cmd |> Cmd.map CounterMsg )

        TimerMsg msg2 ->
            let
                ( timer, cmd ) =
                    Timer.update msg2 model.timer
            in
                ( { model | timer = timer }, cmd |> Cmd.map TimerMsg )


subscriptions model =
    let
        sub1 =
            model.counter |> Counter.subscriptions |> Sub.map CounterMsg

        sub2 =
            model.timer |> Timer.subscriptions |> Sub.map TimerMsg
    in
        Sub.batch [ sub1, sub2 ]


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    ChangeUrl


onUrlChange : Url -> Msg
onUrlChange =
    ChangedUrl
