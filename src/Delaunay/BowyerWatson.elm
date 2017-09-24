module Delaunay.BowyerWatson exposing (..)

import Color
import Delaunay.Triangle exposing (containsPoint, getDelaunayTriangle)
import Edge
import Model exposing (DelaunayTriangle, Edge, Model, Point, Triangle)


calculate : Model -> Model
calculate model =
    { model | triangles = perform model.points [ Delaunay.Triangle.getSuperTriangle ] }


perform : List Point -> List DelaunayTriangle -> List DelaunayTriangle
perform points triangles =
    case List.head points of
        Nothing ->
            triangles
                |> cleanup

        Just point ->
            case List.tail points of
                Nothing ->
                    performOnPoint point triangles

                Just remainingPoints ->
                    performOnPoint point triangles
                        |> perform remainingPoints


cleanup : List DelaunayTriangle -> List DelaunayTriangle
cleanup triangles =
    let
        superTriangle =
            Delaunay.Triangle.getSuperTriangle

        sharesAVertex a b =
            False
    in
    List.filter (Basics.not << sharesAVertex superTriangle) triangles


performOnPoint : Point -> List DelaunayTriangle -> List DelaunayTriangle
performOnPoint point triangles =
    let
        badTriangles =
            getBadTriangles point triangles

        badTrianglesEdges =
            getBadTrianglesEdges point triangles
    in
    --  List.map
    --    (retriangulatePolygonalHole point)
    --  (getBadTrianglesEdges point triangles)
    removeTriangles triangles badTriangles
        |> addReTriangulation point badTrianglesEdges


addReTriangulation : Point -> List Edge -> List DelaunayTriangle -> List DelaunayTriangle
addReTriangulation point edges triangles =
    List.append
        triangles
        (List.map
            (retriangulatePolygonalHole point)
            edges
        )


retriangulatePolygonalHole : Point -> Edge -> DelaunayTriangle
retriangulatePolygonalHole point edge =
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


getBadTrianglesEdges : Point -> List DelaunayTriangle -> List Edge
getBadTrianglesEdges point triangles =
    let
        badTriangles =
            getBadTriangles point triangles
    in
    List.map (\tri -> Delaunay.Triangle.getEdges tri.triangle) badTriangles
        |> List.concat
        |> Edge.getUnique


removeTriangles : List DelaunayTriangle -> List DelaunayTriangle -> List DelaunayTriangle
removeTriangles triangles trianglesToRemove =
    let
        removeTriangle toRemove triangles =
            List.filter
                (\x -> Delaunay.Triangle.compareTriangle toRemove.triangle x.triangle)
                triangles
    in
    List.foldr removeTriangle triangles trianglesToRemove


getGoodTriangles : Point -> List DelaunayTriangle -> List DelaunayTriangle
getGoodTriangles point triangulation =
    let
        isGood point triangle =
            if containsPoint triangle point then
                Nothing
            else
                Just triangle
    in
    List.filterMap (isGood point) triangulation


getBadTriangles : Point -> List DelaunayTriangle -> List DelaunayTriangle
getBadTriangles point triangulation =
    let
        isBad point triangle =
            if containsPoint triangle point then
                Just triangle
            else
                Nothing
    in
    List.filterMap (isBad point) triangulation
