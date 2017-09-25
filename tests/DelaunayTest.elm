module DelaunayTest exposing (suite)

import Color
import Delaunay.Triangle
import Expect
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (Point, Triangle)
import Test exposing (..)


point : Float -> Float -> Point
point x y =
    Point (vec2 x y) (Color.rgb 0 0 0)


triangle : Vec2 -> Vec2 -> Vec2 -> Triangle
triangle a b c =
    let
        color =
            Color.rgb 0 0 0
    in
    Triangle (Point a color) (Point b color) (Point c color)


circle : Float -> Float -> Float -> Model.DelaunayTriangle
circle cx cy radius =
    Model.DelaunayTriangle
        (triangle (vec2 0 0) (vec2 0 0) (vec2 0 0))
        (Model.Circle
            (Just (vec2 cx cy))
            radius
        )


suite : Test
suite =
    describe "Delaunay.Triangle"
        [ describe "Delaunay.Triangle.slope"
            [ test "positive slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.slope (vec2 0 0) (vec2 2 2))
                        (Just 1)
            , test "negative slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.slope (vec2 0 2) (vec2 2 0))
                        (Just -1)
            , test "vertical line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.slope (vec2 0 0) (vec2 0 10))
                        Nothing
            , test "horizontal line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.slope (vec2 0 0) (vec2 10 0))
                        (Just 0)
            , test "same points" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.slope (vec2 0 0) (vec2 0 0))
                        Nothing
            ]
        , describe "Delaunay.Triangle.midpoint"
            [ test "midpoint with leftmost first" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.midpoint (vec2 1 1) (vec2 3 3))
                        (vec2 2 2)
            , test "midpoint with rightmost first" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.midpoint (vec2 3 3) (vec2 1 1))
                        (vec2 2 2)
            , test "same points" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.midpoint (vec2 1 1) (vec2 1 1))
                        (vec2 1 1)
            ]
        , describe "Delaunay.Triangle.perpendicularBisectorSlope"
            [ test "positive slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.perpendicularSlope (vec2 0 0) (vec2 2 2))
                        (Just -1)
            , test "positive slope #2" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.perpendicularSlope (vec2 0 0) (vec2 1 2))
                        (Just (-1 / 2))
            , test "negative slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.perpendicularSlope (vec2 0 2) (vec2 1 0))
                        (Just (1 / 2))
            , test "vertical line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.perpendicularSlope (vec2 0 0) (vec2 0 10))
                        (Just 0)
            , test "horizontal line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.perpendicularSlope (vec2 0 0) (vec2 10 0))
                        Nothing
            ]
        , describe "Delaunay.Triangle.solveSlopeInterceptForB"
            [ test "vertical line, no slope" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.solveSlopeInterceptForB (vec2 5 5) Nothing)
                        Nothing
            , test "horizontal line, slope of 0" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.solveSlopeInterceptForB (vec2 5 5) (Just 0))
                        (Just 5)
            , test "positive sloped line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.solveSlopeInterceptForB (vec2 5 5) (Just 1))
                        (Just 0)
            , test "negative sloped line" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.solveSlopeInterceptForB (vec2 5 5) (Just -1))
                        (Just 10)
            ]
        , describe "Delaunay.Triangle.containsPoint"
            [ test "on origin" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 10 10))
                        True
            , test "on border #1" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 10 0))
                        True
            , test "on border #2" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 0 10))
                        True
            , test "on border #3" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 20 10))
                        True
            , test "on border #4" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 10 20))
                        True
            , test "on border #5" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 5 5))
                        True
            , test "outside #1" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 0 0))
                        False
            , test "outside #2" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 20 0))
                        False
            , test "outside #3" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 0 20))
                        False
            , test "outside #4" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 10) (point 20 20))
                        False
            , test "outside #5" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.containsPoint (circle 10 10 5) (point 50 50))
                        False
            ]

        -- TODO - Fuzz tests
        , describe "Delaunay.Triangle.circumcenter"
            [ test "#1" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.findCircumcenter
                            (vec2 1 6)
                            (vec2 1 4)
                            (vec2 5 4)
                        )
                        (Just (vec2 3 5))
            , test "#2" <|
                \_ ->
                    Expect.equal
                        (Delaunay.Triangle.findCircumcenter
                            (vec2 1 3)
                            (vec2 5 5)
                            (vec2 7 5)
                        )
                        (Just (vec2 6 -2))
            ]
        ]
