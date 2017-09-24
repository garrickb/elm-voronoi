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
            [ test "exact same edges" <|
                \_ ->
                    Expect.equal
                        (Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 0 0) (vec2 2 2)))
                        True
            , test "edges with reverse points" <|
                \_ ->
                    Expect.equal
                        (Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 2 2) (vec2 0 0)))
                        True
            , test "unequal edges #1" <|
                \_ ->
                    Expect.equal
                        (Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 0 0) (vec2 4 2)))
                        False
            , test "unequal edges #2" <|
                \_ ->
                    Expect.equal
                        (Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 2 2) (vec2 4 4)))
                        False
            ]
        ]
