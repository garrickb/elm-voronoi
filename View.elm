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



-- Naive - Find set every pixel's color to closest point.


naiveVoronoi : Model -> List (Svg msg)
naiveVoronoi model =
    List.map (naiveVoronoiRow model)
        (List.map Basics.toFloat (List.range 0 Constants.realSize))


naiveVoronoiRow : Model -> Float -> Svg msg
naiveVoronoiRow model row =
    g
        []
        (List.map
            (naiveVoronoiPoint model row)
            (List.map Basics.toFloat (List.range 0 Constants.realSize))
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
        , x1 (toString (Math.Vector2.getX vecOne.pos))
        , x2 (toString (Math.Vector2.getX vecTwo.pos))
        , y1 (toString (Math.Vector2.getY vecOne.pos))
        , y2 (toString (Math.Vector2.getY vecTwo.pos))
        ]
        []



-- Points - Draws points.


points : Model -> List (Svg msg)
points model =
    List.map point model.points


point : Point -> Svg msg
point point =
    rect
        [ x <| Basics.toString <| Math.Vector2.getX point.pos
        , y <| Basics.toString <| Math.Vector2.getY point.pos
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
