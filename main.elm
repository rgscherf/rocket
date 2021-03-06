module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Mouse
import Keyboard exposing (..)
import Time
import Math.Vector2 exposing (..)
import Debug exposing (..)

import RktGeo exposing (..)
import RktConst exposing (..)
import RktTypes exposing (..)
import RktGraphics exposing (..)


--------------------
-- SIGNALS & ACTIONS
--------------------

signals : Signal Action
signals = Signal.mergeMany
    [ Signal.map (\{x,y} -> Thrust (x,y)) 
        (Signal.sampleOn (Time.fps 30) Keyboard.wasd)
    , Signal.map Tick (Time.fps 60)
    , Signal.map (\k -> if k then Pause else NoOp) Keyboard.space
    ]

type Action
    = Thrust (Int,Int)
    | Tick Float
    | Pause
    | NoOp

main : Signal Element
main = Signal.map render model


--------------------
-- UPDATE
--------------------

update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model
        Thrust delta ->
            let
                newDelta = vec2 (fst delta |> toFloat) 
                                (snd delta |> toFloat)
                newVel = clampVel maxSpeed model.vel newDelta
                collided = getCollision model newVel model.blocks
            in
                if model.paused
                then model
                else updateModel model newVel collided
        Tick delta ->
            let 
                xframeDecay = if getX model.vel > 0
                              then decay * (-1)
                              else decay
                yframeDecay = if getY model.vel > 0
                              then decay * (-1)
                              else decay
                frameDecay = vec2 xframeDecay yframeDecay
                newVel = clampVel maxSpeed model.vel frameDecay
                collided = getCollision model newVel model.blocks
            in 
                if model.paused
                then model
                else if model.finalMap
                    then updateModel model newVel collided
                    else updateModel {model| time <- model.time + delta} newVel collided
        Pause ->
            {model| paused <- not model.paused}

updateModel : Model -> Velocity -> Maybe Block -> Model
updateModel model newVel collided =
    let 
        noCollision = changePos <| {model| vel <- newVel}
    in
        case collided of
            Nothing -> noCollision
            Just b ->
                case b.tile of
                    Wall -> 
                        { model
                        | vel <- bounceDir model newVel b
                        , pos <- add model.pos <|
                            nearestClear model.blocks model.pos model.playerSize 5
                        , slowed <- False
                        }
                    SlowPad -> 
                        if model.slowed
                        then noCollision
                        else changePos <| { model
                                          | vel <- Math.Vector2.scale slowScale newVel
                                          , slowed <- True
                                          }
                    ExitPad ->
                        loadNext model

clampVel : Float -> Vec2 -> Vec2 -> Vec2
clampVel ms vel delta =
    vec2 ( clamp (Basics.negate ms) ms (getX vel + getX delta) )
         ( clamp (Basics.negate ms) ms (getY vel + getY delta) )

changePos : Model -> Model
changePos model =
    let newPos = ( add model.pos model.vel )
    in { model 
       | pos <- newPos 
       , trail <- addTrail model.trail model.pos
       }

getCollision : Model -> Velocity -> List Block -> Maybe Block
getCollision model velocity blocks =
    case blocks of
        [] ->  Nothing
        (b::bs) ->
            if distance b.pos model.pos > (model.playerSize * 3) 
                then getCollision model velocity bs
            else if oneCollide (cirToPoints model.pos model.playerSize) b 
                then Just b
            else getCollision model velocity bs 

nearestClear : List Block -> Vec2 -> Float -> Float -> Vec2
nearestClear blocks position radius delta  =
    let 
        addmodel a b = List.any (oneCollide (cirToPoints (add position <| vec2 a b) radius)) wallBlocks
        wallBlocks = List.filter (\b -> b.tile == Wall) blocks
    in 
        if not <| addmodel delta 0 then
            vec2 delta 0
        else if not <| addmodel -delta 0 then
            vec2 -delta 0
        else if not <| addmodel 0 delta then
            vec2 0 delta
        else if not <| addmodel 0 -delta then
            vec2 0 -delta
        else if not <| addmodel delta delta then
            vec2 delta delta
        else if not <| addmodel -delta delta then
            vec2 -delta delta
        else if not <| addmodel -delta -delta then
            vec2 -delta -delta
        else if not <| addmodel delta -delta then
            vec2 delta -delta
        else nearestClear blocks position radius (delta + 5) 

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
    in 
        List.any isIntersectingAny points

bounceDir : Model -> Velocity -> Block -> Velocity
bounceDir model vel b =
    let 
        halflen = b.length / 2.0
        utop    = add b.pos (vec2 0 halflen)
        dtop    = add b.pos (vec2 0 (-halflen))
        rtop    = add b.pos (vec2 (-halflen) 0)
        ltop    = add b.pos (vec2 halflen 0)
    in if 
           min (distance model.pos utop) (distance model.pos dtop) <=
           min (distance model.pos ltop) (distance model.pos rtop)
       then vec2 (getX vel) (Basics.negate (getY vel))
       else vec2 (Basics.negate (getX vel)) (getY vel)

addTrail : List (Int, Vec2) -> Vec2 -> List (Int, Vec2)
addTrail list pos =
   let shortlist = List.map snd (List.take (trailLength - 1) list)
   in List.map2 (,) [1..100] <| pos :: shortlist


--------------------
-- MODEL
--------------------

initModel : Model
initModel =
    { pos        = vec2 0 0
    , vel        = vec2 0 0
    , angle      = 0
    , viewport   = (windowW, windowH)
    , playerSize = 8
    , blocks     = []
    , debug      = False
    , trail      = []
    , paused     = False
    , slowed     = False
    , time       = 0
    , maps       = [ 
                     map1
                   , map2
                   , map3
                   , finalMap
                   ]
    , finalMap   = False
    }

loadNext : Model -> Model
loadNext model =
    let
        h = List.head model.maps
        t = case List.tail model.maps of
               Nothing -> []
               Just a -> a
    in
        case h of
            Nothing -> initModel
            Just str -> 
                if str == finalMap
                then
                    { initModel
                    | blocks <- snd <| getLevel str
                    , pos <- fst <| getLevel str
                    , maps <- t
                    , time <- model.time
                    , finalMap <- True
                    }
                else
                    { initModel
                    | blocks <- snd <| getLevel str
                    , pos <- fst <| getLevel str
                    , maps <- t
                    , time <- model.time
                    }

model : Signal Model
model = Signal.foldp update (loadNext initModel) signals

