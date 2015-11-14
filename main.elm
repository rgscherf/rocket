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
            let newAngle = if delta /= (0,0)
                           then relativeAngle delta
                           else model.angle
                newDelta = vec2 (fst delta |> toFloat)
                                (snd delta |> toFloat)
                newVel = add model.vel newDelta
            in
            changePos { model 
                          | vel <- checkCollision model model.blocks <| newVel
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
            in changePos { model 
                            | vel <- checkCollision model model.blocks newVel
                         }

changePos : Model -> Model
changePos model =
    let newPos = ( add model.pos model.vel )
    in
        { model
            | pos <- (watch "newPos") newPos
        }

checkCollision : Model -> List Block -> Velocity -> Velocity
checkCollision model blocks vel =
    case blocks of
        [] ->  vel
        (b::bs) ->
            if oneCollide (triToPoints model.playerSize model.angle model.pos) b
            then bounceVel model vel b
            else checkCollision model bs vel 

bounceVel : Model -> Velocity -> Block -> Velocity
bounceVel model vel b =
    let upos = fromTuple (getX b.pos, (b.length/2) + getY b.pos)
        dpos = fromTuple (getX b.pos, (b.length/2) - getY b.pos)
        rpos = fromTuple ((b.length/2) + getX b.pos, getY b.pos)
        lpos = fromTuple ((b.length/2) - getX b.pos, getY b.pos)
        distances = List.map (distance model.pos) [upos, dpos, rpos, lpos]
        allmin = List.foldl min 9999 distances
    in if 
        allmin == (distance model.pos upos) || 
        allmin == (distance model.pos dpos)
            then fromTuple (getX vel, Basics.negate <| getY vel)
            else fromTuple (Basics.negate <| getX vel, getY vel)

-- oneCollide : List Vec2 -> Block -> Bool
-- oneCollide ppoints b =
    -- let bpoints = rectToPoints b.length b.pos
        -- didCollide = List.any (flip isInside (fromVectors ppoints)) bpoints
                  -- || List.any (flip isInside (fromVectors bpoints)) ppoints
    -- in (watch "didcollide") didCollide

oneCollide : List Vec2 -> Block -> Bool
oneCollide ppoints b =
    let
        halflen = b.length / 2
        btop = getY b.pos + halflen
        bbot = getY b.pos - halflen
        bleft = getX b.pos - halflen
        bright = getX b.pos + halflen
        isIntersectingX p = getX p <= bright && getX p >= bleft
        isIntersectingY p = getY p <= btop && getY p >= bbot
        isIntersectingAny p = isIntersectingX p && isIntersectingY p
    in 
        List.any isIntersectingAny ppoints
        

--------------------
-- MODEL
--------------------

init : Model
init =
    { pos = vec2 0 0
    , vel = vec2 0 0
    , angle = 0
    , viewport = (windowW, windowH)
    , playerSize = 30
    , blocks = [ Block 30 (vec2 60 60) purple
               , Block 30 (vec2 90 60) purple
               , Block 30 (vec2 120 60) purple
               , Block 30 (vec2 -60 -30) purple
               , Block 30 (vec2 -60 -60) purple
               , Block 30 (vec2 -60 -90) purple
               , Block 30 (vec2 -400 -100) purple
               , Block 30 (vec2 -400 -80) purple
               , Block 30 (vec2 -400 -50) purple
               , Block 30 (vec2 -370 -50) purple
               , Block 30 (vec2 -340 -50) purple
               , Block 30 (vec2 -340 -80) purple
               , Block 30 (vec2 -340 -100) purple
               , Block 30 (vec2 -370 -100) purple
               , Block 30 (vec2 -400 200) purple
               ]
    , debug = True
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
            , ngon 3 model.playerSize
                |> filled green
                |> rotate model.angle
                |> move (getX model.pos, getY model.pos)
            ]
            ++ List.map drawBlock model.blocks
            ++ debugInfo
        )

drawBlock : Block -> Form
drawBlock b = rect b.length b.length
                |> filled b.color
                |> move (toTuple b.pos)
                
relativeAngle : (Int,Int) -> Float
relativeAngle motion =
    case motion of
        (0,1)   -> pi / 2
        (0,-1)  -> Basics.negate <| pi / 2
        (1,0)   -> 0
        (-1,0)  -> pi
        (1,1)   -> pi * 0.25
        (-1,-1) -> Basics.negate <| pi * 0.75
        (1,-1)  ->  Basics.negate <| pi * 0.25
        (-1,1)  -> pi * 0.75
        otherwise -> 0

