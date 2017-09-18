module Model exposing (..)

import Math.Vector2 exposing (Vec2)
import Random.Pcg exposing (..)
import Svg exposing (..)


type alias Point msg =
    Svg msg


type alias Model =
    { distance : Distance
    , points : List Vec2
    , seed : Seed
    }


type Distance
    = Euclidean
    | Manhattan


init : Model
init =
    { distance = Euclidean
    , points = []
    , seed = initialSeed 1234567890
    }
