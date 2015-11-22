module RktGeo where

import Math.Vector2 exposing (..)
import String 

import RktTypes exposing (..)
import RktConst exposing (..)

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

cirToPoints : Vec2 -> Float -> List Vec2
cirToPoints position radius =
    let radInt = List.map radians [1..360]
    in List.map (rotatedPoint radius position) radInt

buildMapAuto : String -> List Block
buildMapAuto str = buildMap 
                    (Basics.negate ((toFloat windowW) / 2)) 
                    ((toFloat windowH) / 2) 
                    str
buildMap : Float -> Float -> String -> List Block
buildMap x y str =
    let xinc = 30
        yinc = 30
        h = String.left 1 str
        t = String.dropLeft 1 str
    in case h of
        "."  -> buildMap (x + xinc) y t
        "n"  -> buildMap -600 (y - yinc) t
        "0"  -> (Block 30 (vec2 x y)) :: buildMap (x + xinc) y t
        "\n" -> buildMap x y t
        "P"  -> buildMap (x + xinc) y t
        _    -> []

findPlayerAuto : String -> Vec2 
findPlayerAuto str = findPlayer
                        (Basics.negate ((toFloat windowW)/2))
                        ((toFloat windowH)/2)
                        str
findPlayer : Float -> Float -> String -> Vec2
findPlayer x y str =
    let xinc = 30
        yinc = 30
        h = String.left 1 str
        t = String.dropLeft 1 str
    in case h of
        "\n" -> findPlayer x y t
        "n"  -> findPlayer -600 (y - yinc) t
        "P"  -> vec2 x y
        _    -> findPlayer (x + xinc) y t

getLevel : String -> (Vec2, List Block)
getLevel str =
    (findPlayerAuto str, buildMapAuto str)
    
blockMap1 = "
0000000000000000000000000000000000000000n
0.......00000000000....................0n
0........00000000......................0n
0..........0000........................0n
0...........00...........000000........0n
0........................000000........0n
0....00..................000000...00...0n
0..............00........00..00...00...0n
0....00........00........00..00...00...0n
0........................00..00...00...0n
00000000000..............00..00...00...0n
00000000000..............00..00........0n
00.....................0000..0000......0n
00...P.................................0n
00....................0000000000000....0n
00000.0000000..........00000000000000..0n
00000.0000...00.....................0..0n
00000.0000...0000....000000.000000000..0n
00000......000000...0000000............0n
0000000000000000000000000000000000000000n
"

blockMap2 = "
0000000000000000000000000000000000000000n
0......................................0n
0.............00000000000000...........0n
0......000000000000000000000000000.....0n
0...00000000000.............00000......0n
0............................000.......0n
0......................................0n
0...00000...00000000000000000000.......0n
0...00000...000000000000000000000......0n
0...00000...00...........000000000.....0n
0...00000...00....P..........00000.....0n
0...00000...00...............00000.....0n
0...........00...00000000....00000.....0n
0......00...00...00000000....00000.....0n
0......00...00...00000000..............0n
0...........00...00000000..............0n
0...00000...00...00000000....00000.....0n
0............................00000.....0n
0............................00000.....0n
0000000000000000000000000000000000000000n
"
blankMap = "
0000000000000000000000000000000000000000n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0......................................0n
0000000000000000000000000000000000000000n
"
