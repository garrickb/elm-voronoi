module Delaunay.BowyerWatson exposing (..)

import Color
import Delaunay.Triangle exposing (containsPoint, getDelaunayTriangle)
import Edge
import Model exposing (DelaunayTriangle, Edge, Model, Point, Triangle)


perform : Model -> List DelaunayTriangle
perform model =
    getGoodTriangles model.points model.triangles
        |> List.map connectEdgeToPoint


performPoint : Point -> List DelaunayTriangle -> List DelaunayTriangle
performPoint point triangles =
    List.map (connectEdgeToPoint point) (getBadTrianglesEdges [ point ] triangles)


connectEdgeToPoint : Point -> Edge -> DelaunayTriangle
connectEdgeToPoint point edge =
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


getBadTrianglesEdges : List Point -> List DelaunayTriangle -> List Edge
getBadTrianglesEdges points triangles =
    let
        badTriangles =
            getBadTriangles points triangles
    in
    List.map (\tri -> Delaunay.Triangle.getEdges tri.triangle) badTriangles
        |> List.concat
        |> Edge.getUnique


addPoints : Model -> Model
addPoints model =
    model


addPoint : Model -> Model
addPoint model =
    model


removeTriangle : Model -> Triangle -> Model
removeTriangle model triangle =
    { model | triangles = List.filter (\x -> x == getDelaunayTriangle triangle) model.triangles }


getGoodTriangles : List Point -> List DelaunayTriangle -> List DelaunayTriangle
getGoodTriangles points triangulation =
    List.filterMap (isGood points) triangulation


isGood : List Point -> DelaunayTriangle -> Maybe DelaunayTriangle
isGood points triangle =
    if isTriangleBad points triangle then
        Nothing
    else
        Just triangle


getBadTriangles : List Point -> List DelaunayTriangle -> List DelaunayTriangle
getBadTriangles points triangulation =
    List.filterMap (isBad points) triangulation


isBad : List Point -> DelaunayTriangle -> Maybe DelaunayTriangle
isBad points triangle =
    if isTriangleBad points triangle then
        Just triangle
    else
        Nothing


isTriangleBad : List Point -> DelaunayTriangle -> Bool
isTriangleBad points triangle =
    List.any (\x -> x == True) (List.map (containsPoint triangle) points)
