module Delaunay exposing (..)

import Color exposing (rgb)
import ColorHelper exposing (colorToHex)
import Constants exposing (size)
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
    case del.circle.center of
        Nothing ->
            g [] []

        Just center ->
            Svg.circle
                [ cx (Basics.toString (getX center))
                , cy (Basics.toString (getY center))
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
    [ getDelaunayTriangle superTriangle ]


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


slope : Vec2 -> Vec2 -> Maybe Float
slope from to =
    if getX to == getX from then
        -- The slope of a line perpendicular to that with undefined slope is 0
        Nothing
    else
        Just ((getY to - getY from) / (getX to - getX from))


perpendicularBisectorSlope : Vec2 -> Vec2 -> Maybe Float
perpendicularBisectorSlope from to =
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


solveSlopeInterceptForB : Vec2 -> Maybe Float -> Maybe Float
solveSlopeInterceptForB point slope =
    -- Solve y=mx+b for b
    case slope of
        Nothing ->
            -- A vertical line won't intercept y
            Nothing

        Just slope ->
            Just (getY point - slope * getX point)


findCircumcenter : Vec2 -> Vec2 -> Vec2 -> Maybe Vec2
findCircumcenter a b c =
    case circumcenter a b c of
        Nothing ->
            case circumcenter b c a of
                Nothing ->
                    circumcenter c a b

                Just center ->
                    Just center

        Just center ->
            Just center



-- TODO - Cleanup circumcenter function. There's gotta be a cleaner way.


circumcenter : Vec2 -> Vec2 -> Vec2 -> Maybe Vec2
circumcenter a b c =
    let
        -- AB
        slopeAB : Maybe Float
        slopeAB =
            perpendicularBisectorSlope a b

        slopeInterceptAB : Maybe Float
        slopeInterceptAB =
            solveSlopeInterceptForB (midpoint a b) slopeAB

        -- BC
        slopeBC : Maybe Float
        slopeBC =
            perpendicularBisectorSlope b c

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


isInCircle : Vec2 -> Circle -> Bool
isInCircle point circle =
    case circle.center of
        Nothing ->
            False

        Just center ->
            distanceEuclidean point center < circle.radius


triangleToString : Triangle -> String
triangleToString tri =
    List.map pointToString [ tri.a, tri.b, tri.c ]
        |> List.intersperse " "
        |> String.concat
