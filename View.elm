module View exposing (..)

import Constants
import Html exposing (..)
import Html.Attributes
import Html.Events
import Math.Vector2 exposing (Vec2, vec2)
import Model exposing (Model, Point)
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
                    [ Svg.Attributes.name "lines" ]
                    (connectPoints model.points)
                , g
                    [ Svg.Attributes.name "points" ]
                    (points model.points)
                ]
            ]
        , Html.button
            [ Html.Events.onClick (Update.AddPoint (Update.randomVec model)) ]
            [ Html.text "Add Random Point" ]
        ]



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


point : Vec2 -> Point msg
point pos =
    circle
        [ cx <| Basics.toString <| Math.Vector2.getX pos
        , cy <| Basics.toString <| Math.Vector2.getY pos
        , r Constants.dotRadius
        , fill Constants.dotFill
        ]
        []
