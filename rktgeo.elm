module RktGeo where

import Math.Vector2 exposing (..)
import String exposing (..)

import RktTypes exposing (..)

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

cirToPoints : Model -> List Vec2
cirToPoints model =
    let radInt = List.map radians [1..360]
    in List.map (rotatedPoint model.playerSize model.pos) radInt

parse : Float -> Float -> String -> List Block
parse x y str =
    let xinc = 30
        yinc = 30
        h = String.left 1 str
        t = String.dropLeft 1 str
    in case h of
        "." -> parse (x + xinc) y t
        "n" -> parse -600 (y - yinc) t
        "0" -> (Block 30 (vec2 x y)) :: parse (x + xinc) y t
        "\n" -> parse x y t
        _   -> []

blockMap1 = "
0000000000000000000000000000000000000000n
0.......00000000000....................0n
0........00000000......................0n
0..........0000........................0n
0...........00...........000000........0n
0........................000000........0n
0........................000000........0n
0........................00..00........0n
0........................00..00........0n
0........................000000........0n
00000000000............................0n
00000000000............................0n
00000000000............................0n
00000000000............................0n
00000000000............................0n
0000000000000..........................0n
000000000000000........................0n
00000000000000000......................0n
00000000000000000......................0n
0000000000000000000000000000000000000000n
"

