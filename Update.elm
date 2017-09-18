module Update exposing (..)

import Constants
import Math.Vector2 exposing (Vec2, vec2)
import Model exposing (..)
import Random.Pcg exposing (..)


type Msg
    = ToggleDistance
    | AddPoint ( ( Float, Float ), Seed )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDistance ->
            case model.distance of
                Euclidean ->
                    { model | distance = Manhattan }

                Manhattan ->
                    { model | distance = Euclidean }

        AddPoint data ->
            addPoint data model |> updateSeed data


addPoint : ( ( Float, Float ), Seed ) -> Model -> Model
addPoint random model =
    { model | points = getVecFromRandom random :: model.points }


getVecFromRandom : ( ( Float, Float ), Seed ) -> Vec2
getVecFromRandom random =
    vec2
        (toFloat <|
            round
                (Tuple.first <| Tuple.first random)
        )
        (toFloat <|
            round
                (Tuple.second <| Tuple.first random)
        )


updateSeed : ( ( Float, Float ), Seed ) -> Model -> Model
updateSeed random model =
    { model | seed = Tuple.second random }


coordinateGenerator : Generator ( Float, Float )
coordinateGenerator =
    pair
        (float Constants.coordinateBufferZone
            (Constants.realSize - Constants.coordinateBufferZone)
        )
        (float Constants.coordinateBufferZone
            (Constants.realSize - Constants.coordinateBufferZone)
        )


randomVec : Model -> ( ( Float, Float ), Seed )
randomVec model =
    step coordinateGenerator model.seed
