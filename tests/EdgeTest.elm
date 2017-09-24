module EdgeTest exposing (suite)

import Edge exposing (isEqual)
import Expect
import Math.Vector2 exposing (vec2)
import Model exposing (Edge, Point)
import Test exposing (..)


suite : Test
suite =
    describe "Edge"
        [ describe "Edge.isEqual"
            [ test "positive slope" <|
                \_ ->
                    Expect.equal
                        (Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 0 0) (vec2 2 2)))
                        True
            ]
        ]
