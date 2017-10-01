module Voronoi exposing (..)

import Delaunay.Triangle
import Geometry.Edge
import Model exposing (DelaunayTriangle, Edge, Model, Point, VoronoiPolygon)
import Svg exposing (Svg, polyline)
import Svg.Attributes exposing (..)


draw : Model -> List (Svg msg)
draw model =
    let
        voronoi =
            get model
    in
    --List.map Geometry.Edge.draw edges
    List.map drawVoronoi voronoi


drawVoronoi : VoronoiPolygon -> Svg msg
drawVoronoi voronoi =
    polyline
        [ fill "gray"
        , stroke "black"
        , points (toString voronoi)
        ]
        []


toString : VoronoiPolygon -> String
toString voronoi =
    String.concat (List.intersperse " " (List.map (\edge -> Geometry.Edge.toString edge) voronoi.edges))


{-| For each triangle in our triangulation, connect the triangle circumcenter
to it's neighboring triangle's circumcenter.
-}
get : Model -> List VoronoiPolygon
get model =
    getTriangles model.triangles


neighbors : List DelaunayTriangle -> List DelaunayTriangle
neighbors triangles =
    Maybe.withDefault []
        (Maybe.map2
            Delaunay.Triangle.neighbors
            (List.head triangles)
            (List.tail triangles)
        )


getTriangles : List DelaunayTriangle -> List VoronoiPolygon
getTriangles triangles =
    Maybe.withDefault []
        (Maybe.map2
            (getTriangleRecurse [])
            (List.head triangles)
            (List.tail triangles)
        )


getTriangleRecurse : List VoronoiPolygon -> DelaunayTriangle -> List DelaunayTriangle -> List VoronoiPolygon
getTriangleRecurse voronoi triangle triangles =
    let
        neighbors =
            Delaunay.Triangle.neighbors triangle triangles
    in
    List.append
        [ VoronoiPolygon (List.filterMap (connectTriangles triangle) triangles) Nothing ]
        (Maybe.withDefault voronoi
            (Maybe.map2
                (getTriangleRecurse voronoi)
                (List.head triangles)
                (List.tail triangles)
            )
        )


connectTriangles : DelaunayTriangle -> DelaunayTriangle -> Maybe Edge
connectTriangles a b =
    if Delaunay.Triangle.isNeighbor a b then
        Maybe.map2 (\centerA centerB -> Edge (Point centerA Nothing) (Point centerB Nothing))
            a.circle.center
            b.circle.center
    else
        Nothing
