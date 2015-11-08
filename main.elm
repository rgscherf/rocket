import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Mouse
import Keyboard exposing (..)
import Time

--------------------
-- CONSTANTS
--------------------

-- window w and h
windowW : Int
windowW = 1200

windowH : Int
windowH = 600

-- acceleration constant
accel : Float
accel = 1.5 

-- deceleration constant
decay : Float
decay = 0.02


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
                      | vel <- addVelocity model.vel delta
                      , angle <- newAngle
                      }
        Tick _ ->
            let xframeDecay = if fst model.vel > 0
                              then decay * (-1)
                              else decay
                yframeDecay = if snd model.vel > 0
                              then decay * (-1)
                              else decay
            in
            changePos { model 
                      | vel <- ( (fst model.vel) + xframeDecay
                               , (snd model.vel) + yframeDecay
                               )
                      }

changePos : Model -> Model
changePos model =
    { model
    | pos <- ( fst model.pos + fst model.vel
             , snd model.pos + snd model.vel
             )
    }

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
    }

init : Model
init =
    { pos = (0,0)
    , vel = (0,0)
    , angle = 0
    , viewport = (windowW, windowH)
    }

model : Signal Model
model = Signal.foldp update init signals


--------------------
-- VIEW
--------------------

render : Model -> Element
render model =
    collage windowW windowH 
        [ rect (toFloat windowW) (toFloat windowH)
            |> filled lightGrey
        , ngon 3 10
            |> filled green
            |> rotate model.angle
            |> move (fst model.pos, snd model.pos)
        -- , toForm (show model)
        ]

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

-- COLLISION CALCULATIONS
-- collision library represents shapes as lists of vertices
-- so we need to be able to convert our Forms to that.

triRadius : Int -> Float
triRadius length = (/) 2 <| sqrt ((length ^ 2) + ((length/2) ^ 2))

rotatedPoint : Float -> (Float,Float) -> Float
rotatedPoint radius (x,y) angle =
    let newx = (+) x <| cos angle * radius
        newy = (+) y <| sin angle * radius
    in (newx, newy)

-- given a triangle's side length and location
-- give back a list of locations of its points
triPoints : Int -> Float -> (Float, Float) -> List (Float, Float)
triPoints length angle (x,y) =
    let radius = triRadius length
    in map (rotatedPoint radius (x,y)) 
        [angle, angle + (pi * 2/3), angle + (pi * 4/3)] 

