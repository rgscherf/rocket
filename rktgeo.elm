module RktGeo where

import Math.Vector2 exposing (..)

-- for a circle of given radius, find point along the circle
-- at a given angle
rotatedPoint : Float -> Vec2 -> Float -> Vec2
rotatedPoint radius pos angle =
    let newx = (+) (getX pos) <| cos angle * radius
        newy = (+) (getY pos) <| sin angle * radius
    in vec2 newx newy

triRadius : Float -> Float
-- triRadius length = length / (sqrt 3)
triRadius length = length * 0.9

triToPoints : Float -> Float -> Vec2 -> List Vec2
triToPoints length angle pos =
    let radius = triRadius length
    in List.map (rotatedPoint radius pos) 
        [angle, angle + (pi * 2/3), angle + (pi * 4/3)] 

rectRadius : Float -> Float
rectRadius length = 
    let half = length / 2
    in sqrt <| 2 * (half^2)

rectToPoints : Float -> Vec2 -> List Vec2
rectToPoints length pos =
    let radius = rectRadius length
    in List.map (rotatedPoint radius pos)
        [-pi/4, pi/4, pi * 0.75, -pi * 0.75] 
