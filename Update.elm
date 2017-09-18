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
addPosition data model =
    { model
        | points =
            Position (Tuple.first <| Tuple.first data)
                (Tuple.second <| Tuple.first data)
                :: model.points
    }


updateSeed : ( ( Float, Float ), Seed ) -> Model -> Model
updateSeed data model =
    { model | seed = Tuple.second data }


coordinateGenerator : Generator ( Float, Float )
coordinateGenerator =
    pair (float 5 (Constants.realSize - 5)) (float 5 (Constants.realSize - 5))


randomCoordinate : Model -> ( ( Float, Float ), Seed )
randomCoordinate model =
    step coordinateGenerator model.seed
