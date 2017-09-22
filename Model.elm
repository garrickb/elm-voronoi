module Model exposing (..)

import Color exposing (Color)
import Constants exposing (size)
import Math.Vector2 exposing (Vec2, vec2)
import Random.Pcg exposing (..)


type alias Point =
    { pos : Vec2, color : Color }


type alias DelunayTriangle =
    { triangle : Triangle, circle : Circle }


type alias Triangle =
    { a : Point, b : Point, c : Point }


type alias Circle =
    { center : Vec2, radius : Float }


type alias Model =
    { distance : Distance
    , points : List Point
    , triangles : List DelunayTriangle
    , seed : Seed
    }


type Distance
    = Euclidean
    | Manhattan


init : Model
init =
    { distance = Euclidean
    , points =
        [ Point (vec2 (size / 2) (-1 * size)) (Color.rgb 0 0 0)
        , Point (vec2 (-1 * (size / 2)) size) (Color.rgb 0 0 0)
        , Point (vec2 ((3 * size) / 2) size) (Color.rgb 0 0 0)
        ]
    , triangles =
        []
    , seed = initialSeed 3178909195
    }
