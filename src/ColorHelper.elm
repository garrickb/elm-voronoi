-- Borrowed from: eskimoblood/elm-color-extra


module ColorHelper exposing (..)

import Char
import Color exposing (Color)


{-| Turns an RGB Color (ex. [255, 255, 255]) into
a string value (ex. #FFFFFF).
-}
colorToHex : Color -> String
colorToHex cl =
    let
        color =
            Color.toRgb cl

        r =
            min color.red 255

        b =
            min color.blue 255

        g =
            min color.green 255
    in
    "#" ++ toHex r ++ toHex g ++ toHex b


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
