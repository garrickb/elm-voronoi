module DelaunayTest exposing (suite)

import Color
import Delaunay
import Expect
import Math.Vector2 exposing (getX, getY, vec2)
import Model exposing (Point, Triangle)
import Test exposing (..)


point : Float -> Float -> Point
point x y =
    Point (vec2 x y) (Color.rgb 0 0 0)


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
        , describe "Delaunay.perpendicularBisectorSlope"
            [ test "positive slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularBisectorSlope (vec2 0 0) (vec2 2 2))
                        (Just -1)
            , test "positive slope #2" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularBisectorSlope (vec2 0 0) (vec2 1 2))
                        (Just (-1 / 2))
            , test "negative slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularBisectorSlope (vec2 0 2) (vec2 1 0))
                        (Just (1 / 2))
            , test "vertical line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularBisectorSlope (vec2 0 0) (vec2 0 10))
                        (Just 0)
            , test "horizontal line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularBisectorSlope (vec2 0 0) (vec2 10 0))
                        Nothing
            ]
        , describe "Delaunay.solveSlopeInterceptForB"
            [ test "vertical line, no slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.solveSlopeInterceptForB (vec2 5 5) Nothing)
                        Nothing
            , test "horizontal line, slope of 0" <|
                \_ ->
                    Expect.equal
                        (Delaunay.solveSlopeInterceptForB (vec2 5 5) (Just 0))
                        (Just 5)
            , test "positive sloped line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.solveSlopeInterceptForB (vec2 5 5) (Just 1))
                        (Just 0)
            , test "negative sloped line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.solveSlopeInterceptForB (vec2 5 5) (Just -1))
                        (Just 10)
            ]
        , describe "E2E"
            [ test "AB Midpoint" <|
                \_ ->
                    Expect.equal
                        (Delaunay.midpoint (vec2 1 3) (vec2 5 5))
                        (vec2 3 4)
            , test "AB Slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.slope (vec2 1 3) (vec2 5 5))
                        (Just (1 / 2))
            , test "AB perpendicular bisector" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularBisectorSlope (vec2 1 3) (vec2 5 5))
                        (Just -2)
            , test "BC perpendicular bisector" <|
                \_ ->
                    Expect.equal
                        (Delaunay.perpendicularBisectorSlope (vec2 5 5) (vec2 7 5))
                        Nothing
            , test "AB solve intercept" <|
                \_ ->
                    Expect.equal
                        (Delaunay.solveSlopeInterceptForB (vec2 3 4) (Just -2))
                        (Just 10)
            , test "circumcenter" <|
                \_ ->
                    Expect.equal
                        (Delaunay.findCircumcenter (vec2 1 3) (vec2 5 5) (vec2 7 5))
                        (Just (vec2 6 -2))
            ]

        -- TODO - Fuzz tests
        , describe "Delaunay.circumcenter"
            [ test "#1" <|
                \_ ->
                    Expect.equal
                        (Delaunay.findCircumcenter
                            (vec2 1 6)
                            (vec2 1 4)
                            (vec2 5 4)
                        )
                        (Just (vec2 3 5))
            ]
        ]
