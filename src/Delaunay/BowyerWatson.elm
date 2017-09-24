module Delaunay.BowyerWatson exposing (..)

import Delaunay.Triangle exposing (containsPoint, getDelaunayTriangle)
import Model exposing (DelaunayTriangle, Model, Point, Triangle)


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
