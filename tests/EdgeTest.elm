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
        , describe
            "Edge.getUnique"
            [ test "empty" <|
                \_ ->
                    Expect.equal
                        (Edge.getUnique [])
                        []
            , test "no duplicates" <|
                \_ ->
                    Expect.equal
                        (Edge.getUnique
                            [ Edge (vec2 0 0) (vec2 5 5)
                            , Edge (vec2 5 5) (vec2 5 0)
                            ]
                        )
                        [ Edge (vec2 0 0) (vec2 5 5)
                        , Edge (vec2 5 5) (vec2 5 0)
                        ]
            , test "one duplicate" <|
                \_ ->
                    Expect.equal
                        (Edge.getUnique
                            [ Edge (vec2 5 5) (vec2 0 0)
                            , Edge (vec2 0 0) (vec2 5 5)
                            , Edge (vec2 0 0) (vec2 2 2)
                            ]
                        )
                        [ Edge (vec2 0 0) (vec2 2 2) ]
            , test "two duplicates" <|
                \_ ->
                    Expect.equal
                        (Edge.getUnique
                            [ Edge (vec2 0 0) (vec2 1 1)
                            , Edge (vec2 0 0) (vec2 2 2)
                            , Edge (vec2 0 0) (vec2 1 1)
                            , Edge (vec2 0 0) (vec2 2 2)
                            ]
                        )
                        []
            ]
        ]
