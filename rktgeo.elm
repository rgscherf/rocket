module RocketGeometry where

import Math.Vector2 exposing (vec2, Vec2)

-- COLLISION CALCULATIONS
-- collision library represents shapes as lists of vertices
-- so we need to be able to convert our Forms to that.

-- for a circle of given radius, find point along the circle
-- at a given angle
rotatedPoint : Float -> (Float,Float) -> Float -> Vec2
rotatedPoint radius (x,y) angle =
    let newx = (+) x <| cos angle * radius
        newy = (+) y <| sin angle * radius
    in vec2 newx newy

-- TRIANGLE FUNCTIONS
-- get radius of triangle
triRadius : Float -> Float
triRadius length = length / (sqrt 3)

-- list constituent vertices of rectangle
-- given a triangle's side length and location
-- give back a list of locations of its points
triToPoints : Float -> Float -> (Float, Float) -> List Vec2
triToPoints length angle (x,y) =
    let radius = triRadius length
    in List.map (rotatedPoint radius (x,y)) 
        [angle, angle + (pi * 2/3), angle + (pi * 4/3)] 

-- RECT FUNCTIONS
-- get radius of rectangle
rectRadius : Float -> Float
rectRadius length = 
    let half = length / 2
    in sqrt <| 2 * (half^2)

-- list constituent vertices of rectangle
rectToPoints : Float -> (Float,Float) -> List Vec2
rectToPoints length (x,y) =
    let radius = rectRadius length
    in List.map (rotatedPoint radius (x,y))
        [0, pi/2, pi, -pi/2]

dist : (Float, Float) -> (Float, Float) -> Float
dist (xa, ya) (xb, yb) =
    let ws = (xa - xb) ^ 2
        hs = (ya - yb) ^ 2
    in sqrt (ws + hs) 
