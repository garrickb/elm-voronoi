module Update exposing (..)

import Model


type Msg
    = ToggleDistance


update : Msg -> Model.Model -> Model.Model
update msg model =
    case msg of
        ToggleDistance ->
            case model.distance of
                Model.Euclidean ->
                    { model | distance = Model.Manhattan }

                Model.Manhattan ->
                    { model | distance = Model.Euclidean }
