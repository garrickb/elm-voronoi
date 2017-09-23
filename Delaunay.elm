module Delaunay exposing (..)

import Color exposing (rgb)
import ColorHelper exposing (colorToHex)
import Distance exposing (distanceEuclidean)
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (Circle, DelaunayTriangle, Model, Point, Triangle)
import Point exposing (pointToString)
import Svg exposing (Svg, g, polyline)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth)


-- View


drawDelaunay : List DelaunayTriangle -> List (Svg msg)
drawDelaunay del =
    [ drawTriangles del
    , drawCircles del
    ]


drawTriangles : List DelaunayTriangle -> Svg msg
drawTriangles del =
    g
        [ Svg.Attributes.name "triangles" ]
        (List.map
            drawTriangle
            del
        )


drawTriangle : DelaunayTriangle -> Svg msg
drawTriangle del =
    polyline
        [ fill (colorToHex del.triangle.a.color)
        , stroke "black"
        , Svg.Attributes.points (triangleToString del.triangle)
        ]
        []


drawCircles : List DelaunayTriangle -> Svg msg
drawCircles del =
    g
        [ Svg.Attributes.name "circles" ]
        (List.map drawCircle del)


drawCircle : DelaunayTriangle -> Svg msg
drawCircle del =
    Svg.circle
        [ cx (Basics.toString (getX del.circle.center))
        , cy (Basics.toString (getY del.circle.center))
        , r (Basics.toString del.circle.radius)
        , fill "none"
        , stroke "black"
        , strokeWidth "0.25"
        ]
        []



-- Controller


calculateDelaunay : Model -> List DelaunayTriangle
calculateDelaunay model =
    let
        size2 =
            10

        offset =
            vec2 50 50

        superTriangle =
            Triangle
                (Point
                    (Math.Vector2.add offset (vec2 (size2 / 2) (-1 * size2)))
                    (Color.rgb 255 0 0)
                )
                (Point
                    (Math.Vector2.add offset (vec2 ((3 * size2) / 2) size2))
                    (Color.rgb 0 0 255)
                )
                (Point
                    (Math.Vector2.add offset (vec2 (-1 * (size2 / 2)) size2))
                    (Color.rgb 0 255 0)
                )
    in
    [ getDelaunayTriangle superTriangle ]


getDelaunayTriangle : Triangle -> DelaunayTriangle
getDelaunayTriangle tri =
    let
        triCircumcenter =
            getCircumcenter tri
    in
    Circle
        triCircumcenter
        (distanceEuclidean triCircumcenter tri.a.pos)
        |> DelaunayTriangle tri


slope : Vec2 -> Vec2 -> Maybe Float
slope from to =
    if getX to == getX from then
        -- The slope of a line perpendicular to that with undefined slope is 0
        Nothing
    else
        Just ((getY to - getY from) / (getX to - getX from))


perpendicularSlope : Vec2 -> Vec2 -> Maybe Float
perpendicularSlope from to =
    case slope from to of
        Nothing ->
            -- The slope of a line perpendicular to that with undefined slope is 0
            Just 0

        Just slopeResult ->
            if slopeResult == 0 then
                Nothing
            else
                Just (-1 / slopeResult)


midpoint : Vec2 -> Vec2 -> Vec2
midpoint a b =
    vec2 ((getX a + getX b) / 2) ((getY a + getY b) / 2)


solveSlopeInterceptForB : Vec2 -> Maybe Float -> Float
solveSlopeInterceptForB midpoint slope =
    case slope of
        Nothing ->
            -- A vertical line intercepts y at it's y
            getY midpoint

        Just slope ->
            getY midpoint - slope * getX midpoint


getCircumcenter : Triangle -> Vec2
getCircumcenter tri =
    let
        -- Determine midpoints
        midAB =
            midpoint tri.a.pos tri.b.pos

        midBC =
            midpoint tri.b.pos tri.c.pos

        -- We need the negative reciprocal of the slope
        -- to get the slope of the perpendicular bisector
        slopeAB =
            perpendicularSlope tri.a.pos tri.b.pos

        slopeBC =
            perpendicularSlope tri.b.pos tri.c.pos

        -- y = mx + b
        -- solve for b
        bAB =
            solveSlopeInterceptForB midAB slopeAB

        bBC =
            solveSlopeInterceptForB midBC slopeBC

        x =
            (bAB - bBC) / (Maybe.withDefault 0 slopeBC - Maybe.withDefault 0 slopeAB)
    in
    vec2 x ((Maybe.withDefault 0 slopeAB * x) + bAB)


isInCircle : Vec2 -> Circle -> Bool
isInCircle point circle =
    distanceEuclidean point circle.center < circle.radius


triangleToString : Triangle -> String
triangleToString tri =
    List.map pointToString [ tri.a, tri.b, tri.c ]
        |> List.intersperse " "
        |> String.concat
