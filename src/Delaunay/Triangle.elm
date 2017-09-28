module Delaunay.Triangle exposing (..)

import Color exposing (rgb)
import ColorHelper exposing (colorToHex)
import Constants exposing (size)
import Geometry.Distance exposing (distanceEuclidean)
import Geometry.Triangle
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (Circle, DelaunayTriangle, Edge, Model, Point, Triangle)
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


averageTriangleColor : Triangle -> String
averageTriangleColor tri =
    let
        a =
            Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) tri.a.color)

        b =
            Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) tri.b.color)

        c =
            Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) tri.c.color)
    in
    colorToHex
        (Color.rgb
            (round (sqrt (Basics.toFloat ((a.red + b.red + c.red) ^ 2) / 3)))
            (round (sqrt (Basics.toFloat ((a.green + b.green + c.green) ^ 2) / 3)))
            (round (sqrt (Basics.toFloat ((a.blue + b.blue + c.blue) ^ 2) / 3)))
        )


drawTriangle : DelaunayTriangle -> Svg msg
drawTriangle del =
    polyline
        [ fill "none"
        , stroke "black"
        , strokeWidth "1"
        , Svg.Attributes.points (Geometry.Triangle.toString del.triangle)
        ]
        []


drawCircles : List DelaunayTriangle -> Svg msg
drawCircles del =
    g
        [ Svg.Attributes.name "circles" ]
        (List.map drawCircle del)


drawCircle : DelaunayTriangle -> Svg msg
drawCircle del =
    case del.circle.center of
        Nothing ->
            g [] []

        Just center ->
            Svg.circle
                [ cx (Basics.toString (getX center))
                , cy (Basics.toString (getY center))
                , r (Basics.toString del.circle.radius)
                , fill "none"
                , stroke "grey"
                , strokeWidth "0.25"
                ]
                []



-- Controller


defaultTriangles : List DelaunayTriangle
defaultTriangles =
    [ Geometry.Triangle.getDelaunayTriangle
        (Triangle
            (Point (vec2 0 0) Nothing)
            (Point (vec2 0 Constants.size) Nothing)
            (Point (vec2 Constants.size Constants.size) Nothing)
        )
    , Geometry.Triangle.getDelaunayTriangle
        (Triangle
            (Point (vec2 0 0) Nothing)
            (Point (vec2 Constants.size 0) Nothing)
            (Point (vec2 Constants.size Constants.size) Nothing)
        )
    ]


{-| Returns the points comprising the triangle.
-}
getPoints : DelaunayTriangle -> List Point
getPoints triangle =
    [ triangle.triangle.a
    , triangle.triangle.b
    , triangle.triangle.c
    ]


{-| Checks if a DelaunayTriangle's circle contains a point or not.
-}
containsPoint : DelaunayTriangle -> Point -> Bool
containsPoint triangle point =
    case triangle.circle.center of
        Nothing ->
            False

        Just center ->
            distanceEuclidean point.pos center <= triangle.circle.radius
