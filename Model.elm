module Model exposing (..)

import Random.Pcg exposing (..)
import Svg exposing (..)


type alias Point msg =
    Svg msg


type alias Position =
    { x : Float
    , y : Float
    }


type alias Model =
    { distance : Distance
    , points : List Position
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
