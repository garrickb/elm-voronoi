module Geometry.Point exposing (..)

import Color
import ColorHelper exposing (colorToHex)
import Constants exposing (..)
import Math.Vector2 exposing (getX, getY, vec2)
import Model exposing (Model, Point)
import Svg exposing (Svg, line, rect)
import Svg.Attributes exposing (..)


-- View


drawPoints : Model -> List (Svg msg)
drawPoints model =
    List.map draw model.points


draw : Point -> Svg msg
draw point =
    let
        color =
            Color.toRgb (Maybe.withDefault (Color.rgb 255 255 255) point.color)
    in
    Svg.circle
        [ cx <| Basics.toString <| getX point.pos
        , cy <| Basics.toString <| getY point.pos
        , r "4"
        , fill <|
            colorToHex
                (Color.rgb
                    (round (Constants.pointColorMult * Basics.toFloat color.red))
                    (round (Constants.pointColorMult * Basics.toFloat color.green))
                    (round (Constants.pointColorMult * Basics.toFloat color.blue))
                )
        , stroke "black"
        , strokeWidth "0.25"
        ]
        []



-- Controller


toString : Point -> String
toString point =
    String.concat
        (List.intersperse
            ","
            [ Basics.toString (getX point.pos)
            , Basics.toString (getY point.pos)
            ]
        )
