module Delaunay.BowyerWatson exposing (performOnPoint)

import Color
import Constants
import Delaunay.Triangle exposing (containsPoint, getDelaunayTriangle)
import Geometry.Edge
import Geometry.Point exposing (getPoint)
import Model exposing (DelaunayTriangle, Edge, Model, Point, Triangle)


performOnPoint : Point -> List DelaunayTriangle -> List DelaunayTriangle
performOnPoint point triangles =
    let
        defaultTriangles =
            [ getDelaunayTriangle
                (Triangle
                    (getPoint 0 0)
                    (getPoint 0 Constants.size)
                    (getPoint Constants.size Constants.size)
                )
            , getDelaunayTriangle
                (Triangle
                    (getPoint 0 0)
                    (getPoint Constants.size 0)
                    (getPoint Constants.size Constants.size)
                )
            ]
    in
    if triangles == [] then
        goodTriangles point defaultTriangles
            |> retriangulatePolygonalHole point (badTriangleEdges point defaultTriangles)
    else
        goodTriangles point triangles
            |> retriangulatePolygonalHole point (badTriangleEdges point triangles)


retriangulatePolygonalHole : Point -> List Edge -> List DelaunayTriangle -> List DelaunayTriangle
retriangulatePolygonalHole point edges triangles =
    List.append
        triangles
        (List.map
            (retriangulate point)
            edges
        )


{-| Connects the point to the edge, forming a triangle.
-}
retriangulate : Point -> Edge -> DelaunayTriangle
retriangulate point edge =
    getDelaunayTriangle
        (Triangle
            (Point
                edge.a
                (Color.rgb 255 0 0)
            )
            (Point
                edge.b
                (Color.rgb 0 0 255)
            )
            point
        )



-- Utils


{-| Returns ONLY unique edges between all of the bad triangles
found in the triangle list.
-}
badTriangleEdges : Point -> List DelaunayTriangle -> List Edge
badTriangleEdges point triangles =
    List.map
        (\tri -> Delaunay.Triangle.getEdges tri.triangle)
        (badTriangles point triangles)
        |> List.concat
        |> Geometry.Edge.getUnique


{-| Returns triangles that contain the point.
-}
badTriangles : Point -> List DelaunayTriangle -> List DelaunayTriangle
badTriangles point triangulation =
    let
        isBad point triangle =
            if containsPoint triangle point then
                Just triangle
            else
                Nothing
    in
    List.filterMap (isBad point) triangulation


{-| Returns triangles that do not contain the point.
-}
goodTriangles : Point -> List DelaunayTriangle -> List DelaunayTriangle
goodTriangles point triangulation =
    let
        isGood point triangle =
            if containsPoint triangle point then
                Nothing
            else
                Just triangle
    in
    List.filterMap (isGood point) triangulation
