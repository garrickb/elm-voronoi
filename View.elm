module View exposing (..)

import Constants
import Html exposing (..)
import Html.Attributes
import Html.Events
import Model exposing (Model, Point, Position)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Update


view : Model -> Html Update.Msg
view model =
    div [ Html.Attributes.style [ ( "text-align", "center" ) ] ]
        [ h2 []
            [ Html.text "Voronoi" ]
        , div []
            [ points model.points ]
        , Html.button
            [ Html.Events.onClick (Update.AddPosition (Update.randomCoordinate model)) ]
            [ Html.text "Add Random Point" ]
        ]


points : List Position -> Svg msg
points points =
    svg
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
        (List.map point points)


point : Position -> Point msg
point pos =
    circle
        [ cx <| Basics.toString <| pos.x
        , cy <| Basics.toString <| pos.y
        , r Constants.dotRadius
        , fill Constants.dotFill
        ]
        []
