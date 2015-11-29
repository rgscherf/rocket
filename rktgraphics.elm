module RktGraphics where

import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Math.Vector2 exposing (..)
import Text exposing (..)

import RktTypes exposing (..)
import RktConst exposing (..)
import RktGeo exposing (..)

-----------------
-- COLOUR PALETTE
-----------------

background   = lightGrey
paused       = lightRed
wallColor    = purple
slowActive   = orange
slowInactive = darkGrey
player       = green
playerTrail  = lightGreen
textColour = white
exitColour = yellow

-----------------
-- RENDERING
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
                |> filled (if model.paused then paused else background)
            ]
            ++ List.map (drawBlock model) model.blocks
            ++
                [ drawPlayerCir model
                    |> filled player
                ]
            ++ List.map (drawTrail model) model.trail
            ++ debugInfo
            ++
               [ fromString ("TIME : " ++ toString (model.time / 1000))
                   |> Text.height 40
                   |> Text.color textColour
                   |> italic
                 |> text
                 |> move (0, (toFloat windowH/2) - 10)
               ]
        )

drawPlayerCir : Model -> Shape
drawPlayerCir model =
    polygon << List.map toTuple <| cirToPoints model.pos model.playerSize

drawBlock : Model -> Block -> Form
drawBlock m b = 
    let color = case b.tile of
                    Wall -> wallColor
                    SlowPad -> if m.slowed
                               then slowInactive
                               else slowActive
                    ExitPad -> exitColour
    in
    rect b.length b.length
       |> filled color
       |> move (toTuple b.pos)

drawTrail : Model -> (Int, Vec2) -> Form
drawTrail model trail =
    circle model.playerSize
        |> filled playerTrail
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

debugBlock : Block -> List Form
debugBlock b = 
    let
        left    = (getX b.pos - (b.length /2), getY b.pos)
        right   = (getX b.pos + (b.length /2), getY b.pos)
        top     = (getX b.pos, getY b.pos + (b.length / 2))
        bottom  = (getX b.pos, getY b.pos - (b.length / 2))
    in
    [ traced (dashed red)
        <| path (List.map toTuple (rectToPoints b.length b.pos))
    ]
    ++ List.map 
        (\c -> traced (dashed red) <| segment (toTuple b.pos) c) 
        [left, right, top, bottom]
        
                
