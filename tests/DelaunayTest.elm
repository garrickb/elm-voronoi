module DelaunayTest exposing (suite)

import Delaunay.Triangle
import Expect
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Model exposing (Point, Triangle)
import Test exposing (..)


point : Float -> Float -> Point
point x y =
    Point (vec2 x y) Nothing


triangle : Vec2 -> Vec2 -> Vec2 -> Triangle
triangle a b c =
    Triangle
        (Point a Nothing)
        (Point b Nothing)
        (Point c Nothing)


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
    describe "Delaunay"
        [ describe "Delaunay.Triangle.containsPoint"
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
        ]
