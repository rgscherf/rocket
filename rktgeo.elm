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
        putBlock typ = defaultBlock typ x y :: buildMap (x + xinc) y t
    in case h of
        "."  -> buildMap (x + xinc) y t
        "n"  -> buildMap -600 (y - yinc) t
        "0"  -> putBlock Wall 
        "S"  -> putBlock SlowPad
        "E"  -> putBlock ExitPad
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
    

finalMap = "
00000000000000000000000000000000000000..n
0SS0....................0...000000000...n
0SS0.P.0000.000000.SS.0.0.S.............n
0SS00.00SS0......0.SS.0.0.S.000000000...n
0SSSS0SSSS0...0..0....0.0......0.....0..n
0000S0SSSS00000..000000.0000.S.0..0.S.0.n
0..0SSSSSSSSSS0.S..........0......0.S..0n
0.S.00000000000.S.0..00000.0000000.S...0n
0.S.......000.0...0.S..0...0.........000n
0...0000.S.00.0.0.0.S..0.S.0..000.000..0n
0..0....0.S....0.0..S..0...0..........00n
0.....S.0.SS.SS...SS.00000.0.S.000.00..0n
000000..0.S...S.00...0SSSS00....00..00.0n
0......0.0.00...0S0..0SSSS000.0.0S0.0..0n
0.S.000...000.S.0SS0.0000000000.00..0.00n
0..S..0.S...0...0SSS00S0......0........0n
0.S...0..00.0000SSSSSSS0.S.0...S.S.S.S.0n
0..0.S.S..0.0...00000000.S.0000........0n
0..0......0....................000000000n
0000000000000000000000000000000000000000n
"

map1 = "
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0......................................0n
0......................................0n
0.................................EE...0n
0.P...............................EE...0n
0......................................0n
0......................................0n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
"

map2 = "
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
00000000000000000000000000.......0000000n
0000000000000000000000000.........000000n
000000000000000000000000...........00000n
00000000000000000000000.............0000n
0000000000000000000000...............000n
0...........0000.......................0n
0...........0000.......................0n
0...........0000.............00000..E..0n
0.P.........0000............000000.....0n
0...........0000...........0000000000000n
0...........0000..........00000000000000n
00000000....0000.........000000000000000n
00000000................0000000000000000n
00000000...............00000000000000000n
00000000..............000000000000000000n
00000000.............0000000000000000000n
0000000000000000000000000000000000000000n
0000000000000000000000000000000000000000n
"

map3 = "
0000000000000000000000000000000000000000n
0.......0000000000000000....000000000000n
0...P...000000000000000..SS..00000000000n
0.......00000000000000..SSSS..0000000000n
0.......0000000000000..SSSSSS.....000000n
0.......0000000000000..................0n
0.......00000000000000000..00000.......0n
0.......00000000000000000..00000.......0n
0.SSSSS...........0000000..00000.......0n
0.SSSSS...........0000000..00000000...00n
0.SSSSS...........0000000..00000000.E.00n
0.......000000....0000000..00000000...00n
0.......000000....0000000..0000000000000n
00000000000000....0000000..0000000000000n
0.................0000000..0000000000000n
0.................0000000..0000000000000n
0.SSS.00.S.000000000.....SS............0n
0........................SS............0n
0......................................0n
0000000000000000000000000000000000000000n
"
