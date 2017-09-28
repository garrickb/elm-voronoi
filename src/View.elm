module View exposing (..)

import Constants
import Delaunay.Triangle exposing (drawDelaunay)
import Geometry.Point exposing (..)
import Html exposing (..)
import Html.Attributes
import Html.Events
import Model exposing (Circle, DelaunayTriangle, Distance, Model, Point, Triangle)
import Svg exposing (..)
import Svg.Attributes exposing (height, viewBox, width)
import Update


view : Model -> Html Update.Msg
view model =
    div [ Html.Attributes.style [ ( "text-align", "center" ) ] ]
        [ h2 []
            [ Html.text "Delaunay Triangulation using Bowyer Watson algorithm" ]
        , div []
            [ svg
                [ width (Basics.toString Constants.viewSize)
                , height (Basics.toString Constants.viewSize)
                , viewBox
                    ("0 0 "
                        ++ Basics.toString Constants.svgSize
                        ++ " "
                        ++ Basics.toString Constants.svgSize
                    )
                , Html.Attributes.style
                    [ ( "border", "1px solid black" ) ]
                ]
                [ g
                    [ Svg.Attributes.name "delaunay" ]
                    (drawDelaunay model.triangles)
                , g
                    [ Svg.Attributes.name "points" ]
                    (drawPoints model)
                ]
            ]
        , Html.button
            [ Html.Events.onClick (Update.AddPoint (Update.randomPoint model)) ]
            [ Html.text "Add Random Point" ]

        --, Html.button
        --    [ Html.Events.onClick Update.ToggleDistance ]
        --    [ Html.text
        --        (case model.distance of
        --            Model.Euclidean ->
        --                "Switch to Manhattan Distance Formula"
        --            Model.Manhattan ->
        --                "Switch to Chebyshev Distance Formula"
        --            Model.Chebyshev ->
        --                "Switch to Euclidean Distance Formula"
        --        )
        --    ]
        ]
