module Triangles exposing (..)

import Html exposing (..)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL as GL exposing (..)


type alias Vertex =
    { position : Vec2
    , color : Vec3
    }


type alias Varying =
    { vColor : Vec3
    }


main : Program Never {} {}
main =
    Html.beginnerProgram
        { model =
            {}

        -- I have not model
        , view = view
        , update =
            \_ _ -> {}

        -- I update nothing
        }


view : {} -> Html msg
view _ =
    GL.toHtml
        []
        [ boxEntity ]


boxEntity : Entity
boxEntity =
    GL.entity
        vertexShader
        fragmentShader
        boxMesh
        {}


vertexShader : Shader { position : Vec2, color : Vec3 } {} { vColor : Vec3 }
vertexShader =
    [glsl|

  precision mediump float;
  attribute vec2 position;
  attribute vec3 color;
  varying vec3 vColor;

  void main () {
    gl_Position = vec4(position, 0.0, 1.0);
    vColor = color;
  }

  |]


fragmentShader : Shader {} {} Varying
fragmentShader =
    [glsl|

  precision mediump float;
  varying vec3 vColor;

  void main () {
    gl_FragColor = vec4(vColor, 1.);
  }

  |]


boxMesh : Mesh Vertex
boxMesh =
    GL.triangles
        [ ( Vertex (vec2 -1 1) (vec3 1 0 0)
          , Vertex (vec2 1 1) (vec3 0 1 0)
          , Vertex (vec2 -1 -1) (vec3 0 0 1)
          )
        , ( Vertex (vec2 -1 -1) (vec3 1 0 0)
          , Vertex (vec2 1 -1) (vec3 0 1 0)
          , Vertex (vec2 1 1) (vec3 0 0 1)
          )
        ]
