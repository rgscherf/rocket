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
                      | vel <- checkCollision model <| addVelocity model.vel delta
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
            in changePos { model | vel <- checkCollision model newVel }

changePos : Model -> Model
changePos model =
    let newPos = ( fst model.pos + fst model.vel
                 , snd model.pos + snd model.vel
                 )
    in
        { model
        | pos <- newPos
        -- , blocks <- List.map 
            -- (collideBlock model.playerSize model.angle newPos) 
            -- model.blocks
        }

-- every step, check position against internal list of blocks
-- if any collision, reverse velocity
checkCollision : Model -> (Float, Float) -> (Float,Float)
checkCollision model vel =
    let collided = List.any (oneCollide <| triToPoints model.playerSize model.angle model.pos) model.blocks
    in if collided then flipVel vel else vel

oneCollide : List Vec2 -> Block -> Bool
oneCollide ppoints b =
    let bpoints = rectToPoints b.length b.pos
    in List.any (flip isInside (fromVectors ppoints)) bpoints
        || List.any (flip isInside (fromVectors bpoints)) ppoints

flipVel : (Float, Float) -> (Float, Float)
flipVel (x,y) = (negate x, negate y)

-- collideBlock : Float -> Float -> (Float,Float) -> Block -> Block
-- collideBlock length angle (x,y) b =
    -- let tripoints = triPoints length angle (x,y)
        -- rectpoints = rectPoints b.length b.pos
    -- in if List.any (flip isInside (fromVectors tripoints)) rectpoints 
       -- || List.any (flip isInside (fromVectors rectpoints)) tripoints
           -- then {b | color <- red}
           -- else {b | color <- green}

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
    , blocks = [ Block 30 (50,50) purple
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

