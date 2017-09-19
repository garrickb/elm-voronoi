module Model exposing (..)

import Color exposing (Color)
import Math.Vector2 exposing (Vec2)
import Random.Pcg exposing (..)


type alias Point =
    { pos : Vec2, color : Color }


type alias Model =
    { distance : Distance
    , points : List Point
    , seed : Seed
    }


type Distance
    = Euclidean
    | Manhattan


init : Model
init =
    { distance = Euclidean
    , points = []
    , seed = initialSeed 3178909195
    }
