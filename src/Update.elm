module Update exposing (..)

import Color exposing (Color)
import Constants
import Delaunay.BowyerWatson
import Math.Vector2 exposing (Vec2, vec2)
import Model exposing (..)
import Random.Pcg exposing (..)


type Msg
    = ToggleDistance
    | AddPoint ( Point, Seed )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDistance ->
            case model.distance of
                Euclidean ->
                    { model | distance = Manhattan }

                Manhattan ->
                    { model | distance = Chebyshev }

                Chebyshev ->
                    { model | distance = Euclidean }

        AddPoint data ->
            addPoint data model |> updateSeed data


addPoint : ( Point, Seed ) -> Model -> Model
addPoint random model =
    let
        point =
            Tuple.first random
    in
    { model
        | points = point :: model.points
        , triangles = Delaunay.BowyerWatson.performOnPoint point model.triangles
    }


updateSeed : ( Point, Seed ) -> Model -> Model
updateSeed random model =
    { model | seed = Tuple.second random }


randomPoint : Model -> ( Point, Seed )
randomPoint model =
    step pointGenerator model.seed


pointGenerator : Generator Point
pointGenerator =
    map Point coordinateGenerator |> andMap colorGenerator


coordinateGenerator : Generator Vec2
coordinateGenerator =
    map2 vec2
        (float Constants.coordinateBufferZone
            (Constants.size - Constants.coordinateBufferZone)
        )
        (float Constants.coordinateBufferZone
            (Constants.size - Constants.coordinateBufferZone)
        )


randomColor : Model -> ( Color, Seed )
randomColor model =
    step colorGenerator model.seed


colorGenerator : Generator Color
colorGenerator =
    map3 Color.rgb (int 0 255) (int 0 255) (int 0 255)
