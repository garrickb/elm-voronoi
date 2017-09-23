module DelaunayTest exposing (suite)

import Delaunay
import Expect
import Math.Vector2 exposing (vec2)
import Test exposing (..)


suite : Test
suite =
    describe "Delaunay"
        [ describe "Delaunay.slope"
            [ test "positive slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.slope (vec2 0 0) (vec2 2 2))
                        (Just 1)
            , test "negative slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.slope (vec2 0 2) (vec2 2 0))
                        (Just -1)
            , test "vertical line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.slope (vec2 0 0) (vec2 0 10))
                        Nothing
            , test "horizontal line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.slope (vec2 0 0) (vec2 10 0))
                        (Just 0)
            , test "same points" <|
                \_ ->
                    Expect.equal
                        (Delaunay.slope (vec2 0 0) (vec2 0 0))
                        Nothing
            ]
        , describe "Delaunay.midpoint"
            [ test "midpoint with leftmost first" <|
                \_ ->
                    Expect.equal
                        (Delaunay.midpoint (vec2 1 1) (vec2 3 3))
                        (vec2 2 2)
            , test "midpoint with rightmost first" <|
                \_ ->
                    Expect.equal
                        (Delaunay.midpoint (vec2 3 3) (vec2 1 1))
                        (vec2 2 2)
            , test "same points" <|
                \_ ->
                    Expect.equal
                        (Delaunay.midpoint (vec2 1 1) (vec2 1 1))
                        (vec2 1 1)
            ]
        , describe "Delaunay.perpendicularSlope"
            [ test "positive slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularSlope (vec2 0 0) (vec2 2 2))
                        (Just -1)
            , test "positive slope #2" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularSlope (vec2 0 0) (vec2 1 2))
                        (Just (-1 / 2))
            , test "negative slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularSlope (vec2 0 2) (vec2 1 0))
                        (Just (1 / 2))
            , test "vertical line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularSlope (vec2 0 0) (vec2 0 10))
                        (Just 0)
            , test "horizontal line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularSlope (vec2 0 0) (vec2 10 0))
                        Nothing
            ]
        ]
