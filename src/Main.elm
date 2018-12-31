module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Browser.Events as Events
import Html exposing (..)
import Json.Decode as D
import Html exposing (Html, div)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


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
    { x : Float
    , y: Float
    , z : Float
    , direction : Direction
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0.0 0.0 -5.0 Stationary, Cmd.none )



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
update msg model =
    case msg of
        Keypress x ->
            ( updatePosition model (mapDirection x), Cmd.none )

updatePosition : Model -> Direction -> Model
updatePosition model direction =
    case direction of
        Left -> { model | x = model.x - 0.1 }
        Right -> { model | x = model.x + 0.1 }
        Up -> { model | z = model.z + 0.1 }
        Down -> { model | z = model.z - 0.1 }
        Stationary -> model


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
    div [ style "width" "100%"
        , style "height" "100%"
        ]
        [ WebGL.toHtml
            [ width 1200
            , height 600
            , style "display" "block"
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                { perspective = perspective model }
            ]
        ]

perspective : Model -> Mat4
perspective model =
    Mat4.mul
        (Mat4.makePerspective 45 2 0.01 100)
        (Mat4.makeLookAt (vec3 model.x model.y model.z) (vec3 0 0 0) (vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


colorBlack : Vec3
colorBlack =
    vec3 0.1 0.1 0.1


colorPurple : Vec3
colorPurple =
    vec3 0.3 0 0.7


cube : Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
cube origin diagonal color =
    let
        x =
            Vec3.getX origin

        y =
            Vec3.getY origin

        z =
            Vec3.getZ origin

        x2 =
            Vec3.getX diagonal

        y2 =
            Vec3.getY diagonal

        z2 =
            Vec3.getZ diagonal
    in
    [ ( Vertex (vec3 x y z) color
      , Vertex (vec3 x2 y z) color
      , Vertex (vec3 x2 y2 z) color
      )
    , ( Vertex (vec3 x y z) color
      , Vertex (vec3 x y2 z) color
      , Vertex (vec3 x2 y2 z) color
      )
    , ( Vertex (vec3 x y z) color
      , Vertex (vec3 x y2 z) color
      , Vertex (vec3 x y2 z2) color
      )
    , ( Vertex (vec3 x y z) color
      , Vertex (vec3 x y z2) color
      , Vertex (vec3 x y2 z2) color
      )
    , ( Vertex (vec3 x y z) color
      , Vertex (vec3 x2 y z) color
      , Vertex (vec3 x2 y z2) color
      )
    , ( Vertex (vec3 x y z) color
      , Vertex (vec3 x y z2) color
      , Vertex (vec3 x2 y z2) color
      )
    , ( Vertex (vec3 x2 y2 z2) color
      , Vertex (vec3 x y2 z2) color
      , Vertex (vec3 x y z2) color
      )
    , ( Vertex (vec3 x2 y2 z2) color
      , Vertex (vec3 x2 y z2) color
      , Vertex (vec3 x y z2) color
      )
    , ( Vertex (vec3 x2 y2 z2) color
      , Vertex (vec3 x2 y z2) color
      , Vertex (vec3 x2 y z) color
      )
    , ( Vertex (vec3 x2 y2 z2) color
      , Vertex (vec3 x2 y2 z) color
      , Vertex (vec3 x2 y z) color
      )
    , ( Vertex (vec3 x2 y2 z2) color
      , Vertex (vec3 x y2 z2) color
      , Vertex (vec3 x y2 z) color
      )
    , ( Vertex (vec3 x2 y2 z2) color
      , Vertex (vec3 x2 y2 z) color
      , Vertex (vec3 x y2 z) color
      )
    ]


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        (cube (vec3 0 0 0) (vec3 1 1 1) colorPurple
        ++ cube (vec3 -0.5 -0.5 -0.5) (vec3 -1.5 -1.5 -1.5) colorBlack)



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
