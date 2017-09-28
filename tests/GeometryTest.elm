module GeometryTest exposing (suite)

import Expect
import Geometry.Edge
import Geometry.Triangle
import Geometry.Util
import Math.Vector2 exposing (vec2)
import Model exposing (Edge, Point, Triangle)
import Test exposing (..)


suite : Test
suite =
    describe "Geometry"
        [ describe "Geometry.Util"
            [ describe "Geometry.Util.slope"
                [ test "positive slope" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.slope (vec2 0 0) (vec2 2 2))
                            (Just 1)
                , test "negative slope" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.slope (vec2 0 2) (vec2 2 0))
                            (Just -1)
                , test "vertical line" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.slope (vec2 0 0) (vec2 0 10))
                            Nothing
                , test "horizontal line" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.slope (vec2 0 0) (vec2 10 0))
                            (Just 0)
                , test "same points" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.slope (vec2 0 0) (vec2 0 0))
                            Nothing
                ]
            , describe "Geometry.Util.midpoint"
                [ test "midpoint with leftmost first" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.midpoint (vec2 1 1) (vec2 3 3))
                            (vec2 2 2)
                , test "midpoint with rightmost first" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.midpoint (vec2 3 3) (vec2 1 1))
                            (vec2 2 2)
                , test "same points" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.midpoint (vec2 1 1) (vec2 1 1))
                            (vec2 1 1)
                ]
            , describe "Geometry.Util.perpendicularBisectorSlope"
                [ test "positive slope" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.perpendicularSlope (vec2 0 0) (vec2 2 2))
                            (Just -1)
                , test "positive slope #2" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.perpendicularSlope (vec2 0 0) (vec2 1 2))
                            (Just (-1 / 2))
                , test "negative slope" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.perpendicularSlope (vec2 0 2) (vec2 1 0))
                            (Just (1 / 2))
                , test "vertical line" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.perpendicularSlope (vec2 0 0) (vec2 0 10))
                            (Just 0)
                , test "horizontal line" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.perpendicularSlope (vec2 0 0) (vec2 10 0))
                            Nothing
                ]
            , describe "Geometry.Util.solveSlopeInterceptForB"
                [ test "vertical line, no slope" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.solveSlopeInterceptForB (vec2 5 5) Nothing)
                            Nothing
                , test "horizontal line, slope of 0" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.solveSlopeInterceptForB (vec2 5 5) (Just 0))
                            (Just 5)
                , test "positive sloped line" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.solveSlopeInterceptForB (vec2 5 5) (Just 1))
                            (Just 0)
                , test "negative sloped line" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Util.solveSlopeInterceptForB (vec2 5 5) (Just -1))
                            (Just 10)
                ]
            ]
        , describe
            "Geometry.Edge"
            [ describe "Geometry.Edge.isEqual"
                [ test "exact same edges" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 0 0) (vec2 2 2)))
                            True
                , test "edges with reverse points" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 2 2) (vec2 0 0)))
                            True
                , test "unequal edges #1" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 0 0) (vec2 4 2)))
                            False
                , test "unequal edges #2" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Edge.isEqual (Edge (vec2 0 0) (vec2 2 2)) (Edge (vec2 2 2) (vec2 4 4)))
                            False
                ]
            , describe
                "Geometry.Edge.getUnique"
                [ test "empty" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Edge.getUnique [])
                            []
                , test "no duplicates" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Edge.getUnique
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
                            (Geometry.Edge.getUnique
                                [ Edge (vec2 5 5) (vec2 0 0)
                                , Edge (vec2 0 0) (vec2 5 5)
                                , Edge (vec2 0 0) (vec2 2 2)
                                ]
                            )
                            [ Edge (vec2 0 0) (vec2 2 2) ]
                , test "two duplicates" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Edge.getUnique
                                [ Edge (vec2 0 0) (vec2 1 1)
                                , Edge (vec2 0 0) (vec2 2 2)
                                , Edge (vec2 0 0) (vec2 1 1)
                                , Edge (vec2 0 0) (vec2 2 2)
                                ]
                            )
                            []
                ]
            ]
        , describe
            "Geometry.Triangle"
            [ describe "Geometry.Triangle.circumcenter"
                -- TODO - Fuzz tests
                [ test "#1" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Triangle.findCircumcenter
                                (Triangle
                                    (Point (vec2 1 6) Nothing)
                                    (Point (vec2 1 4) Nothing)
                                    (Point (vec2 5 4) Nothing)
                                )
                            )
                            (Just (vec2 3 5))
                , test "#2" <|
                    \_ ->
                        Expect.equal
                            (Geometry.Triangle.findCircumcenter
                                (Triangle
                                    (Point (vec2 1 3) Nothing)
                                    (Point (vec2 5 5) Nothing)
                                    (Point (vec2 7 5) Nothing)
                                )
                            )
                            (Just (vec2 6 -2))
                ]
            ]
        ]
