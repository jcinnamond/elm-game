module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Browser.Events as Events
import Html exposing (..)
import Json.Decode as D


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type Direction
    = Up
    | Down
    | Left
    | Right
    | Stationary


type alias Model =
    { key : String
    , direction : Direction
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Stationary, Cmd.none )



-- Update


type Msg
    = Keypress String


mapDirection : String -> Direction
mapDirection s =
    case s of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Stationary


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Keypress x ->
            ( Model x (mapDirection x), Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onKeyDown (D.map Keypress keyDecoder)
        , Events.onKeyUp (D.map (\_ -> Keypress "") (D.lazy (\_ -> keyDecoder)))
        ]


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string



-- View


showDirection : Direction -> String
showDirection x =
    case x of
        Up ->
            "^"

        Down ->
            "\\/"

        Left ->
            "<-"

        Right ->
            "->"

        Stationary ->
            "(not moving)"


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text ("Direction is " ++ showDirection model.direction) ]
        , p [] [ text ("Key is " ++ model.key) ]
        ]
