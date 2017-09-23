module ConnectPoints exposing (..)

import Model exposing (Model, Point)
import Point exposing (defaultPoint, drawLine)
import Svg exposing (Svg)


-- View


connectPoints : Model -> List (Svg msg)
connectPoints model =
    if List.isEmpty model.points then
        []
    else
        List.append (connectPoint model)
            (connectPoints { model | points = List.drop 1 model.points })


connectPoint : Model -> List (Svg msg)
connectPoint model =
    connect (List.head model.points) (List.tail model.points)


connect : Maybe Point -> Maybe (List Point) -> List (Svg msg)
connect current remaining =
    List.map (drawLine (current |> Maybe.withDefault defaultPoint))
        (remaining |> Maybe.withDefault [])
