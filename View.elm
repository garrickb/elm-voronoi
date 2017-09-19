module View exposing (..)

import Char
import Color exposing (Color)
import Constants
import Html exposing (..)
import Html.Attributes
import Html.Events
import Math.Vector2 exposing (Vec2, vec2)
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Update


view : Model -> Html Update.Msg
view model =
    div [ Html.Attributes.style [ ( "text-align", "center" ) ] ]
        [ h2 []
            [ Html.text "Hey, it does a thing!" ]
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
                    (naiveVoronoi model.points (Constants.realSize ^ 2))
                , g
                    [ Svg.Attributes.name "points" ]
                    (points model.points)
                ]
            ]
        , Html.button
            [ Html.Events.onClick (Update.AddPoint (Update.randomVec model)) ]
            [ Html.text "Add Random Point" ]
        ]



-- Naive - Find set every pixel's color to closest point


naiveVoronoi : List Vec2 -> Int -> List (Svg msg)
naiveVoronoi points index =
    if index == -1 then
        []
    else if List.isEmpty points then
        []
    else
        List.append (naiveVoronoiPoint (intToPosition index)) (naiveVoronoi points (index - 1))


intToPosition : Int -> Vec2
intToPosition i =
    vec2 (Basics.toFloat (i % Constants.realSize)) (Basics.toFloat (i // Constants.realSize))


naiveVoronoiPoint : Vec2 -> List (Svg msg)
naiveVoronoiPoint pos =
    [ drawPixel pos (Color.rgb 0 255 0) ]


closestPoint : Vec2 -> Vec2
closestPoint point =
    vec2 0 0


drawPixel : Vec2 -> Color -> Svg msg
drawPixel position pointColor =
    rect
        [ x <| Basics.toString <| Math.Vector2.getX position
        , y <| Basics.toString <| Math.Vector2.getY position
        , width "1"
        , height "1"
        , fill (colorToHex pointColor)
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


points : List Vec2 -> List (Svg msg)
points points =
    List.map point points


point : Vec2 -> Svg msg
point pos =
    circle
        [ cx <| Basics.toString <| Math.Vector2.getX pos
        , cy <| Basics.toString <| Math.Vector2.getY pos
        , r Constants.dotRadius
        , fill Constants.dotFill
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
