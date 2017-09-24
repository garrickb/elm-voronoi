module NaiveVoronoi exposing (..)

import ColorHelper exposing (colorToHex)
import Constants
import Distance exposing (..)
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (..)
import Point exposing (defaultPoint)
import Svg exposing (..)
import Svg.Attributes exposing (..)


naiveVoronoi : Model -> List (Svg msg)
naiveVoronoi model =
    List.map (naiveVoronoiRow model)
        (List.map Basics.toFloat (List.range 0 Constants.size))


naiveVoronoiRow : Model -> Float -> Svg msg
naiveVoronoiRow model row =
    g
        []
        (List.map
            (naiveVoronoiPoint model row)
            (List.map Basics.toFloat (List.range 0 Constants.size))
        )


naiveVoronoiPoint : Model -> Float -> Float -> Svg msg
naiveVoronoiPoint model row col =
    drawVoronoiPoint
        (Point (vec2 row col)
            (closestPoint model (vec2 row col)).color
        )


closestPoint : Model -> Vec2 -> Point
closestPoint model point =
    Maybe.withDefault defaultPoint <|
        List.head <|
            List.sortBy (distance model.distance point) model.points


drawVoronoiPoint : Point -> Svg msg
drawVoronoiPoint point =
    rect
        [ x <| Basics.toString <| getX point.pos
        , y <| Basics.toString <| getY point.pos
        , width "1"
        , height "1"
        , fill <| colorToHex point.color
        ]
        []
