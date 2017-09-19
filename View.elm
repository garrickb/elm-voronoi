module View exposing (..)

import Char
import Color exposing (Color)
import Constants
import Html exposing (..)
import Html.Attributes
import Html.Events
import Math.Vector2 exposing (Vec2, vec2)
import Model exposing (Distance, Model, Point)
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
                        ++ Basics.toString Constants.realSize
                        ++ " "
                        ++ Basics.toString Constants.realSize
                    )
                , Html.Attributes.style
                    [ ( "border", "1px solid black" ) ]
                ]
                [ g
                    [ Svg.Attributes.name "naive" ]
                    (naiveVoronoi model (Constants.realSize ^ 2))
                , g
                    [ Svg.Attributes.name "points" ]
                    (points model.points)
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



-- Naive - Find set every pixel's color to closest point


naiveVoronoi : Model -> Int -> List (Svg msg)
naiveVoronoi model index =
    if index == -1 then
        []
    else if List.isEmpty model.points then
        []
    else
        List.append (naiveVoronoiPoint <| intToPoint model index) (naiveVoronoi model (index - 1))


intToPoint : Model -> Int -> Point
intToPoint model i =
    Point
        (vec2 (Basics.toFloat (i % Constants.realSize))
            (Basics.toFloat (i // Constants.realSize))
        )
        (closestPoint model
            (vec2 (Basics.toFloat (i % Constants.realSize))
                (Basics.toFloat (i // Constants.realSize))
            )
        ).color


naiveVoronoiPoint : Point -> List (Svg msg)
naiveVoronoiPoint point =
    [ drawVoronoiPoint point ]


closestPoint : Model -> Vec2 -> Point
closestPoint model point =
    Maybe.withDefault
        (Point (vec2 0 0) (Color.rgb 0 0 0))
    <|
        List.head <|
            List.sortBy (distance model.distance point) model.points


distance : Distance -> Vec2 -> Point -> Float
distance distForm a b =
    case distForm of
        Model.Euclidean ->
            sqrt
                (((Math.Vector2.getX a - Math.Vector2.getX b.pos) ^ 2)
                    + ((Math.Vector2.getY a - Math.Vector2.getY b.pos) ^ 2)
                )

        Model.Manhattan ->
            abs (Math.Vector2.getX a - Math.Vector2.getX b.pos)
                + abs (Math.Vector2.getY a - Math.Vector2.getY b.pos)


drawVoronoiPoint : Point -> Svg msg
drawVoronoiPoint point =
    rect
        [ x <| Basics.toString <| Math.Vector2.getX point.pos
        , y <| Basics.toString <| Math.Vector2.getY point.pos
        , width "1"
        , height "1"
        , fill <| colorToHex point.color
        ]
        []



-- connectAll - Doing this one just for some practice. Connects all nodes together.


connectPoints : List Vec2 -> List (Svg msg)
connectPoints points =
    if List.isEmpty points then
        []
    else
        List.append (connectPoint points) (connectPoints (List.drop 1 points))


connectPoint : List Vec2 -> List (Svg msg)
connectPoint point =
    connect (List.head point) (List.tail point)


connect : Maybe Vec2 -> Maybe (List Vec2) -> List (Svg msg)
connect current remaining =
    List.map (drawLine (current |> Maybe.withDefault (vec2 0 0))) (remaining |> Maybe.withDefault [])


drawLine : Vec2 -> Vec2 -> Svg msg
drawLine vecOne vecTwo =
    line
        [ stroke "grey"
        , strokeWidth Constants.lineWidth
        , x1 (toString (Math.Vector2.getX vecOne))
        , x2 (toString (Math.Vector2.getX vecTwo))
        , y1 (toString (Math.Vector2.getY vecOne))
        , y2 (toString (Math.Vector2.getY vecTwo))
        ]
        []



-- Points


points : List Point -> List (Svg msg)
points points =
    List.map point points


point : Point -> Svg msg
point point =
    circle
        [ cx <| Basics.toString <| Math.Vector2.getX point.pos
        , cy <| Basics.toString <| Math.Vector2.getY point.pos
        , r Constants.dotRadius
        , stroke "black"
        , strokeWidth Constants.dotBorder
        , fill <| colorToHex point.color
        ]
        []



-- Util


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
