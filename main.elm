import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Mouse
import Time

--------------------
-- CONSTANTS
--------------------

-- window w and h
windowW : Int
windowW = 1500

windowH : Int
windowH = 600

-- canvas scalar
-- change with window size to maintain
-- proper level size...?
-- scalar : Float
-- scalar = 15

-- acceleration constant
accel : Float
accel = 4 

-- deceleration constant
decay : Float
decay = 0.01


--------------------
-- SIGNALS & ACTIONS
--------------------

signals : Signal Action
signals = Signal.mergeMany
    [ Signal.map Target Mouse.position
    , Signal.map Thrust Mouse.isDown
    , Signal.map Tick (Time.fps 30)
    ]

type Action
    = Target (Int,Int)
    | Thrust Bool
    | Tick Float

main : Signal Element
main = Signal.map render model


--------------------
-- UPDATE
--------------------

update : Action -> Model -> Model
update action model =
    case action of
        Target (x,y) ->
            let rmx = toFloat x - (toFloat (fst model.viewport) / 2.0)
                rmy = (toFloat (snd model.viewport) / 2.0) - toFloat y
            in
                { model | target <- (rmx, rmy) }
        Thrust b ->
            { model | thrusting <- b }
        Tick _ ->
            let velocity = velocityCalc model
            in
                { model
                | pos <- ( fst model.pos + fst velocity
                         , snd model.pos + snd velocity
                         )
                , vel <- velocity 
                , lastAngle <- relativeAngle model.pos model.target
                }

velocityCalc : Model -> (Float, Float)
velocityCalc model =
    let velocity = thrustVelocity model
    in
        if model.thrusting
            then ( accel * (fst <| velocity)
                 , accel * (snd <| velocity)
                 )
            else ( max 0 ((fst model.vel) - decay)
                 , max 0 ((snd model.vel) - decay)
                 )

relativeAngle : (Float, Float) -> (Float, Float) -> Float
relativeAngle (shipx, shipy) (mx, my) =
    atan2 (my - shipy) (mx - shipx)


--------------------
-- MODEL
--------------------

type alias Model =
    { pos : (Float, Float)
    , vel : (Float, Float)
    , target : (Float, Float)
    , viewport : (Int, Int)
    , thrusting : Bool
    , lastAngle : Float
    }

init : Model
init =
    { pos = (-250,-250)
    , vel = (0,0)
    , target = (0,0)
    , viewport = (windowW, windowH)
    , thrusting = False
    , lastAngle = 0
    }

model : Signal Model
model = Signal.foldp update init signals


--------------------
-- VIEW
--------------------

render : Model -> Element
render model =
    collage windowW windowH 
        [ ngon 3 10
            |> filled green
            |> rotate (relativeAngle model.pos model.target)
            |> move (fst model.pos, snd model.pos)
        , toForm (show model)
        ]

thrustVelocity : Model -> (Float, Float)
thrustVelocity model =
    let angle = relativeAngle model.pos model.target
    in (getVelX angle, getVelY angle)

getVelX : Float -> Float
getVelX angle =
    (((abs angle / pi) * 2) - 1 ) * (-1)

getVelY : Float -> Float
getVelY angle =
   let a = abs angle
       halfpi = pi / 2
   in if a > halfpi
        then (((a - halfpi) / halfpi) * (-2)) + 1
        else ((a / halfpi) * (-2)) + 1
