module Point exposing (..)

import Color
import ColorHelper exposing (colorToHex)
import Constants exposing (..)
import Math.Vector2 exposing (getX, getY, vec2)
import Model exposing (Model, Point)
import Svg exposing (Svg, line, rect)
import Svg.Attributes exposing (..)


-- View


points : Model -> List (Svg msg)
points model =
    List.map point model.points


point : Point -> Svg msg
point point =
    Svg.circle
        [ cx <| Basics.toString <| getX point.pos
        , cy <| Basics.toString <| getY point.pos
        , r "4"
        , fill <|
            colorToHex
                (Color.rgb
                    (round (Constants.pointColorMult * Basics.toFloat (Color.toRgb point.color).red))
                    (round (Constants.pointColorMult * Basics.toFloat (Color.toRgb point.color).green))
                    (round (Constants.pointColorMult * Basics.toFloat (Color.toRgb point.color).blue))
                )
        , stroke "black"
        , strokeWidth "0.25"
        ]
        []


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



-- Controller


defaultPoint : Point
defaultPoint =
    Point (vec2 0 0) (Color.rgb 255 255 255)


pointToString : Point -> String
pointToString point =
    String.concat
        (List.intersperse
            ","
            [ Basics.toString (getX point.pos)
            , Basics.toString (getY point.pos)
            ]
        )
