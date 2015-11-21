module RktGraphics where

import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Math.Vector2 exposing (..)

import RktTypes exposing (..)
import RktConst exposing (..)
import RktGeo exposing (..)


-----------------
-- BASIC RENDER
-----------------

render : Model -> Element
render model =
    let 
        debugInfo = if model.debug
                    then drawDebug model
                    else []
    in
        collage windowW windowH 
        (
            [ rect (toFloat windowW) (toFloat windowH)
                |> filled lightGrey
            , drawPlayerCir model
                |> filled green
            ]
            ++ List.map drawBlock model.blocks
            ++ debugInfo
        )

drawPlayerCir : Model -> Shape
drawPlayerCir model =
    polygon << List.map toTuple <| cirToPoints model.pos model.playerSize

drawBlock : Block -> Form
drawBlock b = rect b.length b.length
               |> filled purple
               |> move (toTuple b.pos)


-----------------
-- STILL NEEDED??
-----------------

relativeAngle : (Int,Int) -> Float
relativeAngle motion =
    case motion of
        (0,1)     -> pi / 2
        (0,-1)    -> Basics.negate <| pi / 2
        (1,0)     -> 0
        (-1,0)    -> pi
        (1,1)     -> pi * 0.25
        (-1,-1)   -> Basics.negate <| pi * 0.75
        (1,-1)    -> Basics.negate <| pi * 0.25
        (-1,1)    -> pi * 0.75
        otherwise -> 0

-----------------
-- DEBUG COMMANDS
-----------------

drawDebug : Model -> List Form
drawDebug model =
    [   cirToPoints model.pos model.playerSize
        |> List.map toTuple
        >> path
        >> traced (dashed red)
    ]
    ++ (List.concat (List.map debugBlock model.blocks))

debugBlock : Block -> List Form
debugBlock b = 
    let
        left   = (getX b.pos - (b.length /2), getY b.pos)
        right  = (getX b.pos + (b.length /2), getY b.pos)
        top    = (getX b.pos, getY b.pos + (b.length / 2))
        bottom = (getX b.pos, getY b.pos - (b.length / 2))
    in
    [ traced (dashed red)
        <| path (List.map toTuple (rectToPoints b.length b.pos))
    ]
    ++ List.map 
        (\c -> traced (dashed red) <| segment (toTuple b.pos) c) 
        [left, right, top, bottom]
                
