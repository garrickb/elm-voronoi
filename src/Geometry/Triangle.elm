module Geometry.Triangle exposing (..)

import Geometry.Distance
import Geometry.Edge
import Geometry.Point
import Geometry.Util
import Math.Vector2 exposing (Vec2, vec2)
import Model exposing (Edge, Point, Triangle)


{-| Connects a point to an edge, forming a triangle.
-}
retriangulate : Point -> Edge -> Triangle
retriangulate point edge =
    Triangle
        (Point edge.a Nothing)
        (Point edge.b Nothing)
        point


{-| Try every combination of points for the circumcenter.
-}
findCircumcenter : Triangle -> Maybe Vec2
findCircumcenter triangle =
    let
        a =
            triangle.a

        b =
            triangle.b

        c =
            triangle.c
    in
    case circumcenter (Triangle a c b) of
        Nothing ->
            case circumcenter (Triangle b a c) of
                Nothing ->
                    circumcenter (Triangle b c a)

                Just center ->
                    Just center

        Just center ->
            Just center


{-| Finds the circumcenter of a triangle (given it's three points).
-}
circumcenter : Triangle -> Maybe Vec2
circumcenter triangle =
    let
        a =
            triangle.a.pos

        b =
            triangle.b.pos

        c =
            triangle.c.pos

        -- AB
        slopeAB : Maybe Float
        slopeAB =
            Geometry.Util.perpendicularSlope a b

        slopeInterceptAB : Maybe Float
        slopeInterceptAB =
            Geometry.Util.solveSlopeInterceptForB (Geometry.Util.midpoint a b) slopeAB

        -- BC
        slopeBC : Maybe Float
        slopeBC =
            Geometry.Util.perpendicularSlope b c

        slopeInterceptBC : Maybe Float
        slopeInterceptBC =
            Geometry.Util.solveSlopeInterceptForB (Geometry.Util.midpoint b c) slopeBC

        -- Solve for x
        x : Maybe Float
        x =
            Maybe.map4 (\siAB siBC slBC slAB -> (siAB - siBC) / (slBC - slAB))
                slopeInterceptAB
                slopeInterceptBC
                slopeBC
                slopeAB
    in
    Maybe.map3 (\x sAB siAB -> vec2 x (sAB * x + siAB))
        x
        slopeAB
        slopeInterceptAB


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
toString : Triangle -> String
toString tri =
    List.map Geometry.Point.toString [ tri.a, tri.b, tri.c, tri.a ]
        |> List.intersperse " "
        |> String.concat


{-| Returns true if the triangle contains the edge passed as a parameter.
-}
triangleHasEdge : Triangle -> Edge -> Bool
triangleHasEdge triangle edge =
    List.any (Geometry.Edge.isEqual edge) (getEdges triangle)


{-| Returns true if the triangles have all three edges in common.
-}
compareTriangle : Triangle -> Triangle -> Bool
compareTriangle a b =
    if List.all (triangleHasEdge a) (getEdges b) then
        True
    else
        False


{-| Turns a triangle into a DelaunayTriangle which
contains information about the circumcenter and radius.
-}
getDelaunayTriangle : Triangle -> Model.DelaunayTriangle
getDelaunayTriangle tri =
    let
        circCenter =
            findCircumcenter tri
    in
    Model.Circle
        circCenter
        (Geometry.Distance.distanceEuclidean (Maybe.withDefault (vec2 0 0) circCenter) tri.a.pos)
        |> Model.DelaunayTriangle tri
