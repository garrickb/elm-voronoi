module Delaunay.BowyerWatson exposing (performOnPoint)

import Delaunay.Triangle exposing (containsPoint, getDelaunayTriangle)
import Geometry.Edge
import Geometry.Triangle
import Model exposing (DelaunayTriangle, Edge, Model, Point, Triangle)


performOnPoint : Point -> List DelaunayTriangle -> List DelaunayTriangle
performOnPoint point triangles =
    if triangles == [] then
        goodTriangles point Delaunay.Triangle.defaultTriangles
            |> retriangulatePolygonalHole point (badTriangleEdges point Delaunay.Triangle.defaultTriangles)
    else
        goodTriangles point triangles
            |> retriangulatePolygonalHole point (badTriangleEdges point triangles)


retriangulatePolygonalHole : Point -> List Edge -> List DelaunayTriangle -> List DelaunayTriangle
retriangulatePolygonalHole point edges triangles =
    List.append
        triangles
        (List.map
            (\edge ->
                getDelaunayTriangle (Geometry.Triangle.retriangulate point edge)
            )
            edges
        )



-- Utils


{-| Returns ONLY unique edges between all of the bad triangles
found in the triangle list.
-}
badTriangleEdges : Point -> List DelaunayTriangle -> List Edge
badTriangleEdges point triangles =
    List.map
        (\tri -> Geometry.Triangle.getEdges tri.triangle)
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
