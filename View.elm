module View exposing (..)

import Char
import Color exposing (Color)
import Constants
import Html exposing (..)
import Html.Attributes
import Html.Events
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (Circle, DelunayTriangle, Distance, Model, Point, Triangle)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Update


view : Model -> Html Update.Msg
view model =
    div [ Html.Attributes.style [ ( "text-align", "center" ) ] ]
        [ h2 []
            [ Html.text "Voronoi Diagram" ]
        , div []
            [ svg
                [ width (Basics.toString Constants.viewSize)
                , height (Basics.toString Constants.viewSize)
                , viewBox
                    ("0 0 "
                        ++ Basics.toString Constants.size
                        ++ " "
                        ++ Basics.toString Constants.size
                    )
                , Html.Attributes.style
                    [ ( "border", "1px solid black" ) ]
                ]
                [ g
                    [ Svg.Attributes.name "naiveVoronoi" ]
                    (naiveVoronoi model)
                , g
                    [ Svg.Attributes.name "points" ]
                    (points model)
                ]
            ]
        , Html.button
            [ Html.Events.onClick (Update.AddPoint (Update.randomPoint model)) ]
            [ Html.text "Add Random Point" ]
        , Html.button
            [ Html.Events.onClick Update.ToggleDistance ]
            [ Html.text
                (case model.distance of
                    Model.Euclidean ->
                        "Switch to Manhattan Distance Formula"

                    Model.Manhattan ->
                        "Switch to Euclidean Distance Formula"
                )
            ]
        ]



-- Delaunay Triangulation - Calculate using the Bowyer-Watson algorithm.


drawDelaunay : List DelunayTriangle -> List (Svg msg)
drawDelaunay del =
    [ drawTriangles del
    , drawCircles del
    ]


drawTriangles : List DelunayTriangle -> Svg msg
drawTriangles del =
    g
        [ Svg.Attributes.name "triangles" ]
        (List.map
            drawTriangle
            del
        )


drawCircles : List DelunayTriangle -> Svg msg
drawCircles del =
    g
        [ Svg.Attributes.name "circles" ]
        (List.map drawCircle del)


drawCircle : DelunayTriangle -> Svg msg
drawCircle del =
    Svg.circle
        [ cx (Basics.toString (getX del.circle.center))
        , cy (Basics.toString (getY del.circle.center))
        , r (Basics.toString del.circle.radius)
        ]
        []


drawTriangle : DelunayTriangle -> Svg msg
drawTriangle del =
    polyline
        [ fill (colorToHex del.triangle.a.color)
        , stroke "black"
        , Svg.Attributes.points (getTriPoints del.triangle)
        ]
        []


getTriPoints : Triangle -> String
getTriPoints tri =
    List.map pointToString [ tri.a, tri.b, tri.c ]
        |> List.intersperse " "
        |> String.concat


pointToString : Point -> String
pointToString point =
    String.concat
        (List.intersperse
            ","
            [ Basics.toString (getX point.pos)
            , Basics.toString (getY point.pos)
            ]
        )


calcCircle : Triangle -> DelunayTriangle
calcCircle tri =
    DelunayTriangle tri
        (Circle
            (getCircumcenter tri)
            (distanceEuclidean (getCircumcenter tri) tri.a.pos)
        )


circumcenterX : Triangle -> Float
circumcenterX tri =
    yIntercept tri.a.pos tri.b.pos
        / (slope tri.b.pos tri.c.pos - slope tri.a.pos tri.b.pos)


getCircumcenter : Triangle -> Vec2
getCircumcenter tri =
    vec2 (circumcenterX tri)
        ((slope tri.a.pos tri.b.pos * circumcenterX tri)
            + yIntercept tri.a.pos tri.b.pos
        )


midpoint : Vec2 -> Vec2 -> Vec2
midpoint a b =
    vec2 ((getX a + getX b) / 2) ((getY a + getY b) / 2)


yIntercept : Vec2 -> Vec2 -> Float
yIntercept a b =
    getY (midpoint a b) - slope a b * getX (midpoint a b)


slope : Vec2 -> Vec2 -> Float
slope from to =
    (getY to - getY from) / (getX to - getX from)


isInCircle : Vec2 -> Circle -> Bool
isInCircle point circle =
    distanceEuclidean point circle.center < circle.radius



-- Naive - Set every pixel's color to closest point.


naiveVoronoi : Model -> List (Svg msg)
naiveVoronoi model =
    List.map (naiveVoronoiRow model)
        (List.map Basics.toFloat (List.range 0 Constants.size))


naiveVoronoiRow : Model -> Float -> Svg msg
naiveVoronoiRow model row =
    g
        []
        (List.map
            (naiveVoronoiPoint model row)
            (List.map Basics.toFloat (List.range 0 Constants.size))
        )


naiveVoronoiPoint : Model -> Float -> Float -> Svg msg
naiveVoronoiPoint model row col =
    drawVoronoiPoint
        (Point (vec2 row col)
            (closestPoint model (vec2 row col)).color
        )


closestPoint : Model -> Vec2 -> Point
closestPoint model point =
    Maybe.withDefault defaultPoint <|
        List.head <|
            List.sortBy (distance model.distance point) model.points


distance : Distance -> Vec2 -> Point -> Float
distance distForm a b =
    case distForm of
        Model.Euclidean ->
            distanceEuclidean a b.pos

        Model.Manhattan ->
            distanceManhattan a b.pos


drawVoronoiPoint : Point -> Svg msg
drawVoronoiPoint point =
    rect
        [ x <| Basics.toString <| getX point.pos
        , y <| Basics.toString <| getY point.pos
        , width "1"
        , height "1"
        , fill <| colorToHex point.color
        ]
        []



-- connectAll - Connects all nodes together.


connectPoints : Model -> List (Svg msg)
connectPoints model =
    if List.isEmpty model.points then
        []
    else
        List.append (connectPoint model)
            (connectPoints { model | points = List.drop 1 model.points })


connectPoint : Model -> List (Svg msg)
connectPoint model =
    connect (List.head model.points) (List.tail model.points)


connect : Maybe Point -> Maybe (List Point) -> List (Svg msg)
connect current remaining =
    List.map (drawLine (current |> Maybe.withDefault defaultPoint))
        (remaining |> Maybe.withDefault [])


drawLine : Point -> Point -> Svg msg
drawLine vecOne vecTwo =
    line
        [ stroke "grey"
        , strokeWidth Constants.lineWidth
        , x1 (toString (getX vecOne.pos))
        , x2 (toString (getX vecTwo.pos))
        , y1 (toString (getY vecOne.pos))
        , y2 (toString (getY vecTwo.pos))
        ]
        []



-- Points - Draws points.


points : Model -> List (Svg msg)
points model =
    List.map point model.points


point : Point -> Svg msg
point point =
    rect
        [ x <| Basics.toString <| getX point.pos
        , y <| Basics.toString <| getY point.pos
        , width "1"
        , height "1"
        , fill <|
            colorToHex
                (Color.rgb
                    (round (Constants.pointColorMult * Basics.toFloat (Color.toRgb point.color).red))
                    (round (Constants.pointColorMult * Basics.toFloat (Color.toRgb point.color).green))
                    (round (Constants.pointColorMult * Basics.toFloat (Color.toRgb point.color).blue))
                )
        ]
        []



-- Util


distanceEuclidean : Vec2 -> Vec2 -> Float
distanceEuclidean a b =
    sqrt
        (((getX a - getX b) ^ 2)
            + ((getY a - getY b) ^ 2)
        )


distanceManhattan : Vec2 -> Vec2 -> Float
distanceManhattan a b =
    abs (getX a - getX b) + abs (getY a - getY b)


defaultPoint : Point
defaultPoint =
    Point (vec2 0 0) (Color.rgb 255 255 255)


colorToHex : Color -> String
colorToHex cl =
    let
        { red, green, blue, alpha } =
            Color.toRgb cl
    in
    "#" ++ toHex red ++ toHex green ++ toHex blue


toHex : Int -> String
toHex n =
    let
        hex =
            toRadix n
    in
    if String.length hex == 1 then
        "0" ++ hex
    else
        hex


toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                toString c
            else
                String.fromChar <| Char.fromCode (87 + c)
    in
    if n < 16 then
        getChr n
    else
        toRadix (n // 16) ++ getChr (n % 16)
