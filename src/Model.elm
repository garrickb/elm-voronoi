module Model exposing (..)

import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2)
import Random.Pcg exposing (..)


type alias Point =
    { pos : Vec2, color : Maybe Color }


type alias Edge =
    { a : Vec2, b : Vec2 }


type alias DelaunayTriangle =
    { triangle : Triangle, circle : Circle }


type alias Triangle =
    { a : Point, b : Point, c : Point }


type alias Circle =
    { center : Maybe Vec2, radius : Float }


type alias Model =
    { distance : Distance
    , points : List Point
    , triangles : List DelaunayTriangle
    , seed : Seed
    }


type Distance
    = Euclidean
    | Manhattan
    | Chebyshev


init : Model
init =
    { distance = Euclidean
    , points = []
    , triangles = []
    , seed = initialSeed 3178909195
    }
