-- Borrowed from: eskimoblood/elm-color-extra


module ColorHelper exposing (..)

import Char
import Color exposing (Color)


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
