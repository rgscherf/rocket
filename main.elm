module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Mouse
import Keyboard exposing (..)
import Time
import Math.Vector2 exposing (..)
import Collision2D exposing (..)
import Debug exposing (..)

import RktGeo exposing (..)
import RktConst exposing (..)
import RktDebug exposing (..)
import RktTypes exposing (..)


--------------------
-- SIGNALS & ACTIONS
--------------------

signals : Signal Action
signals = Signal.mergeMany
    [ Signal.map (\{x,y} -> Thrust (x,y)) 
        (Signal.sampleOn (Time.fps 15) Keyboard.wasd)
    , Signal.map Tick (Time.fps 60)
    ]

type Action
    = Thrust (Int,Int)
    | Tick Float

main : Signal Element
main = Signal.map render model


--------------------
-- UPDATE
--------------------

update : Action -> Model -> Model
update action model =
    case action of
        Thrust delta ->
            -- TODO: 1) max speed. 2) this code be ugly!
            let newAngle = if delta /= (0,0)
                           then relativeAngle delta
                           else model.angle
                newDelta = vec2 (fst delta |> toFloat)
                                (snd delta |> toFloat)
                newVel = add model.vel newDelta
            in
            changePos { model 
                          | vel <- checkCollision model newVel model.blocks 
                          , angle <- newAngle
                      }
        Tick _ ->
            let xframeDecay = if getX model.vel > 0
                              then decay * (-1)
                              else decay
                yframeDecay = if getY model.vel > 0
                              then decay * (-1)
                              else decay
                newVel = vec2 (getX model.vel + xframeDecay) 
                              (getY model.vel + yframeDecay)
            in changePos {model| vel <- checkCollision model newVel model.blocks} 

changePos : Model -> Model
changePos model =
    let newPos = ( add model.pos model.vel )
    in { model | pos <- (watch "newPos") newPos }

checkCollision : Model -> Velocity -> List Block -> Velocity
checkCollision model vel blocks =
    case blocks of
        [] ->  vel
        (b::bs) ->
            if distance b.pos model.pos > (model.playerSize * 2) 
            then checkCollision model vel bs
            else if oneCollide (cirToPoints model) b 
                then bounceDir model vel b 
                else checkCollision model vel bs 

oneCollide : List Vec2 -> Block -> Bool
oneCollide points b =
    let
        halflen = b.length / 2.0
        btop    = getY b.pos + halflen
        bbot    = getY b.pos - halflen
        bleft   = getX b.pos - halflen
        bright  = getX b.pos + halflen
        isIntersectingX p   = ((getX p) <= bright) && ((getX p) >= bleft)
        isIntersectingY p   = ((getY p) <= btop) && ((getY p) >= bbot)
        isIntersectingAny p = (isIntersectingX p) && (isIntersectingY p)
        didCollide = List.any isIntersectingAny points
    in (watch "didcollide") didCollide

bounceDir : Model -> Velocity -> Block -> Velocity
bounceDir model vel b =
    let halflen = b.length / 2.0
        upos = add b.pos (vec2 0 halflen)
        dpos = add b.pos (vec2 0 (-halflen))
        rpos = add b.pos (vec2 (-halflen) 0)
        lpos = add b.pos (vec2 halflen 0)
    in if 
        min (distance model.pos upos) (distance model.pos dpos) <
        min (distance model.pos lpos) (distance model.pos rpos)
        then vec2 (getX vel) (Basics.negate (getY vel))
        else vec2 (Basics.negate (getX vel)) (getY vel)


--------------------
-- MODEL
--------------------

init : Model
init =
    { pos        = vec2 0 0
    , vel        = vec2 0 0
    , angle      = 0
    , viewport   = (windowW, windowH)
    , playerSize = 15
    -- , blocks = [ Block 30 (vec2 60 60) purple
               -- , Block 30 (vec2 90 60) purple
               -- , Block 30 (vec2 120 60) purple
               -- , Block 30 (vec2 -60 -30) purple
               -- , Block 30 (vec2 -60 -60) purple
               -- , Block 30 (vec2 -60 -90) purple
               -- , Block 30 (vec2 -400 -110) purple
               -- , Block 30 (vec2 -400 -80) purple
               -- , Block 30 (vec2 -400 -50) purple
               -- , Block 30 (vec2 -370 -50) purple
               -- , Block 30 (vec2 -340 -50) purple
               -- , Block 30 (vec2 -340 -80) purple
               -- , Block 30 (vec2 -340 -110) purple
               -- , Block 30 (vec2 -370 -110) purple
               -- , Block 30 (vec2 -400 200) purple
               -- ]
    , blocks = buildMap -600 300 blockMap1
    , debug = False
    }

model : Signal Model
model = Signal.foldp update init signals


--------------------
-- VIEW
--------------------

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
    polygon << List.map toTuple <| cirToPoints model

drawBlock : Block -> Form
drawBlock b = rect b.length b.length
               |> filled purple
               |> move (toTuple b.pos)
                
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

