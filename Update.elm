module Update exposing (..)

import Constants
import Model exposing (..)
import Random.Pcg exposing (..)


type Msg
    = ToggleDistance
    | AddPosition ( ( Float, Float ), Seed )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDistance ->
            case model.distance of
                Model.Euclidean ->
                    { model | distance = Model.Manhattan }

                Model.Manhattan ->
                    { model | distance = Model.Euclidean }

        AddPosition data ->
            addPosition data model |> updateSeed data


addPosition : ( ( Float, Float ), Seed ) -> Model -> Model
addPosition random model =
    { model | points = getPositionFromRandom random :: model.points }


getPositionFromRandom : ( ( Float, Float ), Seed ) -> Position
getPositionFromRandom random =
    Position
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


randomCoordinate : Model -> ( ( Float, Float ), Seed )
randomCoordinate model =
    step coordinateGenerator model.seed
