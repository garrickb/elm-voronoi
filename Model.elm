module Model exposing (..)


type alias Model =
    { distance : Distance }


type Distance
    = Euclidean
    | Manhattan


init : Model
init =
    { distance = Euclidean }
