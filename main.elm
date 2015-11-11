import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Mouse
import Keyboard exposing (..)
import Time
import Math.Vector2 exposing (Vec2)
import Collision2D exposing (..)
import Debug exposing (..)

import RocketGeometry exposing (..)
import RocketConstants exposing (..)



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
            in
            changePos { model 
                      | vel <- checkCollision model model.blocks <| addVelocity model.vel delta
                      , angle <- newAngle
                      }
        Tick _ ->
            let xframeDecay = if fst model.vel > 0
                              then decay * (-1)
                              else decay
                yframeDecay = if snd model.vel > 0
                              then decay * (-1)
                              else decay
                newVel = ( (fst model.vel) + xframeDecay
                         , (snd model.vel) + yframeDecay
                         )
            in changePos { model | vel <- checkCollision model model.blocks newVel }

changePos : Model -> Model
changePos model =
    let newPos = ( fst model.pos + fst model.vel
                 , snd model.pos + snd model.vel
                 )
    in
        { model
        | pos <- (watch "newPos") newPos
        -- , blocks <- List.map 
            -- (collideBlock model.playerSize model.angle newPos) 
            -- model.blocks
        }

checkCollision : Model -> List Block -> (Float,Float) -> (Float,Float)
checkCollision model blocks vel =
    case blocks of
        [] ->  vel
        (b::bs) ->
            if oneCollide (triToPoints model.playerSize model.angle model.pos) b
            then decideVel model vel b
            else checkCollision model bs vel 

decideVel : Model -> (Float,Float) -> Block -> (Float,Float)
decideVel model vel block =
   if (fst model.pos) < ((block.length / 2) + fst block.pos ) 
    || (fst model.pos) > ((block.length / 2) - fst block.pos )
    then (fst vel, negate <| snd vel)
    else (negate <| fst vel, snd vel)

oneCollide : List Vec2 -> Block -> Bool
oneCollide ppoints b =
    let bpoints = rectToPoints b.length b.pos
    in List.any (flip isInside (fromVectors ppoints)) bpoints
        || List.any (flip isInside (fromVectors bpoints)) ppoints

addVelocity : (Float, Float) -> (Int, Int) -> (Float, Float)
addVelocity (mx, my) (x, y) =
    let calcx = mx + toFloat x
        calcy = my + toFloat y
        maxvel = 5
    in ( max (negate maxvel) (min calcx maxvel)
       , max (negate maxvel) (min calcy maxvel)
       )


--------------------
-- MODEL
--------------------

type alias Model =
    { pos : (Float, Float)
    , vel : (Float, Float)
    , angle : Float
    , viewport : (Int, Int)
    , playerSize : Float
    , blocks : List Block
    }

init : Model
init =
    { pos = (0,0)
    , vel = (0,0)
    , angle = 0
    , viewport = (windowW, windowH)
    , playerSize = playerSize
    , blocks = [ Block 30 (60,60) purple
               , Block 30 (90,60) purple
               , Block 30 (120,60) purple
               , Block 30 (-60,-30) purple
               , Block 30 (-60,-60) purple
               , Block 30 (-60,-90) purple
               , Block 30 (-400,-100) purple
               , Block 30 (-400, -80) purple
               , Block 30 (-400, -50) purple
               , Block 30 (-370,-50) purple
               , Block 30 (-340,-50) purple
               , Block 30 (-340, -80) purple
               , Block 30 (-340, -100) purple
               , Block 30 (-370, -100) purple
               , Block 30 (-400, 200) purple
               ]
    }

model : Signal Model
model = Signal.foldp update init signals


--------------------
-- OBSTACLES
--------------------

type alias Block =
    { length : Float
    , pos : (Float, Float)
    , color : Color
    } 
    
--------------------
-- VIEW
--------------------

render : Model -> Element
render model =
    collage windowW windowH 
    (
        [ rect (toFloat windowW) (toFloat windowH)
            |> filled lightGrey
        , ngon 3 model.playerSize
            |> filled green
            |> rotate model.angle
            |> move (fst model.pos, snd model.pos)
        ]
        ++ List.map drawBlock model.blocks
    )

drawBlock : Block -> Form
drawBlock b = rect b.length b.length
                |> filled b.color
                |> move b.pos
                
relativeAngle : (Int,Int) -> Float
relativeAngle motion =
    case motion of
        (0,1)   -> pi / 2
        (0,-1)  -> negate <| pi / 2
        (1,0)   -> 0
        (-1,0)  -> pi
        (1,1)   -> pi * 0.25
        (-1,-1) -> negate <| pi * 0.75
        (1,-1)  ->  negate <| pi * 0.25
        (-1,1)  -> pi * 0.75
        otherwise -> 0

