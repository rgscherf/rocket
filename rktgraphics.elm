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
                |> filled (if model.paused then charcoal else lightGrey)
            , drawPlayerCir model
                |> filled green
            ]
            ++ List.map (drawTrail model) model.trail
            ++ List.map drawBlock model.blocks
            ++ debugInfo
        )

drawPlayerCir : Model -> Shape
drawPlayerCir model =
    polygon << List.map toTuple <| cirToPoints model.pos model.playerSize

drawBlock : Object -> Form
drawBlock block = 
    case block of
        Blk b ->
            rect b.length b.length
                       |> filled purple
                       |> move (toTuple b.pos)
        -- Slow block ->
            -- rect b.length b.length
                       -- |> filled purple
                       -- |> move (toTuple b.pos)
        Exit b ->
            rect b.length b.length
                       |> filled purple
                       |> move (toTuple b.pos)

drawTrail : Model -> (Int, Vec2) -> Form
drawTrail model trail =
    circle model.playerSize
        |> filled lightGreen
        |> alpha (1 - (toFloat (fst trail) / toFloat trailLength))
        |> move (toTuple <| snd trail)

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

debugBlock : Object -> List Form
debugBlock block = 
    let
        left b   = (getX b.pos - (b.length /2), getY b.pos)
        right b  = (getX b.pos + (b.length /2), getY b.pos)
        top b    = (getX b.pos, getY b.pos + (b.length / 2))
        bottom b = (getX b.pos, getY b.pos - (b.length / 2))
    in
    case block of
        Blk b ->
            [ traced (dashed red)
                <| path (List.map toTuple (rectToPoints b.length b.pos))
            ]
            ++ List.map 
                (\c -> traced (dashed red) <| segment (toTuple b.pos) c) 
                [left b, right b, top b, bottom b]
        Exit b ->
            [ traced (dashed red)
                <| path (List.map toTuple (rectToPoints b.length b.pos))
            ]
            ++ List.map 
                (\c -> traced (dashed red) <| segment (toTuple b.pos) c) 
                [left b, right b, top b, bottom b]
                
                
