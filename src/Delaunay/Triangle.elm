module Delaunay.Triangle exposing (..)

import Color exposing (rgb)
import ColorHelper exposing (colorToHex)
import Constants exposing (size)
import Distance exposing (distanceEuclidean)
import Edge
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (Circle, DelaunayTriangle, Edge, Model, Point, Triangle)
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


averageTriangleColor : Triangle -> String
averageTriangleColor tri =
    let
        a =
            Color.toRgb tri.a.color

        b =
            Color.toRgb tri.b.color

        c =
            Color.toRgb tri.c.color
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


getMinAndMax : List Point -> (Point -> Float) -> { min : Float, max : Float }
getMinAndMax points getVal =
    let
        sorted =
            List.sortBy (\x -> getVal x) points

        min : Point
        min =
            Maybe.withDefault
                (Point (vec2 0 0) (Color.rgb 0 0 0))
                (List.head sorted)

        max : Point
        max =
            Maybe.withDefault
                (Point (vec2 Constants.size Constants.size) (Color.rgb 0 0 0))
                (List.reverse sorted |> List.head)
    in
    { min = getVal min, max = getVal max }


getSuperTriangle : DelaunayTriangle
getSuperTriangle =
    -- TODO - Generate super triangle based on points already in the model
    let
        superTriangle =
            Triangle
                (Point
                    (vec2 (Constants.svgSize / 2) (-1 * Constants.svgSize))
                    (Color.rgb 255 0 0)
                )
                (Point
                    (vec2 ((3 * Constants.svgSize) / 2) Constants.svgSize)
                    (Color.rgb 0 0 255)
                )
                (Point
                    (vec2 (-1 * (Constants.svgSize / 2)) Constants.svgSize)
                    (Color.rgb 0 255 0)
                )
    in
    getDelaunayTriangle superTriangle


{-| Returns the points comprising the triangle.
-}
getPoints : DelaunayTriangle -> List Point
getPoints triangle =
    [ triangle.triangle.a
    , triangle.triangle.b
    , triangle.triangle.c
    ]


{-| Turns a triangle into a DelaunayTriangle which
contains information about the circumcenter and radius.
-}
getDelaunayTriangle : Triangle -> DelaunayTriangle
getDelaunayTriangle tri =
    let
        circCenter =
            findCircumcenter tri.a.pos tri.b.pos tri.c.pos
    in
    Circle
        circCenter
        (distanceEuclidean (Maybe.withDefault (vec2 0 0) circCenter) tri.a.pos)
        |> DelaunayTriangle tri


{-| Finds the slope between two points.
-}
slope : Vec2 -> Vec2 -> Maybe Float
slope from to =
    if getX to == getX from then
        -- The slope of a line perpendicular to that with undefined slope is 0
        Nothing
    else
        Just ((getY to - getY from) / (getX to - getX from))


{-| Finds the slope of the perpendicular bisector
for two points.
-}
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


{-| Finds the midpoint between two points.
-}
midpoint : Vec2 -> Vec2 -> Vec2
midpoint a b =
    vec2 ((getX a + getX b) / 2) ((getY a + getY b) / 2)


{-| Solves y=mx+b for b
-}
solveSlopeInterceptForB : Vec2 -> Maybe Float -> Maybe Float
solveSlopeInterceptForB point slope =
    case slope of
        Nothing ->
            -- A vertical line won't intercept y
            Nothing

        Just slope ->
            Just (getY point - slope * getX point)


{-| Try every combination of points for the circumcenter.
-}
findCircumcenter : Vec2 -> Vec2 -> Vec2 -> Maybe Vec2
findCircumcenter a b c =
    -- Try every combination just in case...
    -- TODO - clean this up
    case circumcenter a c b of
        Nothing ->
            case circumcenter b a c of
                Nothing ->
                    case circumcenter b c a of
                        Nothing ->
                            case circumcenter c a b of
                                Nothing ->
                                    circumcenter c b a

                                Just center ->
                                    Just center

                        Just center ->
                            Just center

                Just center ->
                    Just center

        Just center ->
            Just center


{-| Finds the circumcenter of a triangle (given it's three points).
-}
circumcenter : Vec2 -> Vec2 -> Vec2 -> Maybe Vec2
circumcenter a b c =
    -- TODO - Cleanup function. There's gotta be a cleaner way.
    let
        -- AB
        slopeAB : Maybe Float
        slopeAB =
            perpendicularSlope a b

        slopeInterceptAB : Maybe Float
        slopeInterceptAB =
            solveSlopeInterceptForB (midpoint a b) slopeAB

        -- BC
        slopeBC : Maybe Float
        slopeBC =
            perpendicularSlope b c

        slopeInterceptBC : Maybe Float
        slopeInterceptBC =
            solveSlopeInterceptForB (midpoint b c) slopeBC

        x : Maybe Float
        x =
            case slopeInterceptAB of
                Nothing ->
                    Nothing

                Just siAB ->
                    case slopeInterceptBC of
                        Nothing ->
                            Nothing

                        Just siBC ->
                            case slopeBC of
                                Nothing ->
                                    Nothing

                                Just slBC ->
                                    case slopeAB of
                                        Nothing ->
                                            Nothing

                                        Just slAB ->
                                            Just ((siAB - siBC) / (slBC - slAB))
    in
    case x of
        Nothing ->
            Nothing

        Just x ->
            case slopeAB of
                Nothing ->
                    Nothing

                Just sAB ->
                    case slopeInterceptAB of
                        Nothing ->
                            Nothing

                        Just siAB ->
                            Just
                                (vec2 x
                                    (sAB * x + siAB)
                                )


{-| Checks if a DelaunayTriangle's circle contains a point or not.
-}
containsPoint : DelaunayTriangle -> Point -> Bool
containsPoint triangle point =
    case triangle.circle.center of
        Nothing ->
            False

        Just center ->
            distanceEuclidean point.pos center <= triangle.circle.radius


{-| Returns the three edges comprising the triangle.
-}
getEdges : Triangle -> List Edge
getEdges triangle =
    [ Edge triangle.a.pos triangle.b.pos
    , Edge triangle.b.pos triangle.c.pos
    , Edge triangle.a.pos triangle.c.pos
    ]


{-| Returns the points of a triangle in String form for drawing
the triangle in Svg

returns: [x1,y1 x2,y2 x3,y3 x1,y1]

-}
triangleToString : Triangle -> String
triangleToString tri =
    List.map pointToString [ tri.a, tri.b, tri.c, tri.a ]
        |> List.intersperse " "
        |> String.concat


{-| Returns true if the triangle contains the edge passed as a parameter.
-}
triangleHasEdge : Triangle -> Edge -> Bool
triangleHasEdge triangle edge =
    List.any (Edge.isEqual edge) (getEdges triangle)


{-| Returns true if the triangles have all three edges in common.
-}
compareTriangle : Triangle -> Triangle -> Bool
compareTriangle a b =
    if List.all (triangleHasEdge a) (getEdges b) then
        True
    else
        False
