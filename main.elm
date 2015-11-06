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
windowW = 700

windowH : Int
windowH = 600

-- canvas scalar
-- change with window size to maintain
-- proper level size...?
-- scalar : Float
-- scalar = 15

-- acceleration constant
accel : Float
accel = 0.25 

-- deceleration constant
decay : Float
decay = 0.12


--------------------
-- SIGNALS & ACTIONS
--------------------

signals : Signal Action
signals = Signal.mergeMany
    [ Signal.map Target Mouse.position
    , Signal.map Thrust Mouse.isDown
    , Signal.map Tick (Time.fps 60)
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
                }

velocityCalc : Model -> (Float, Float)
velocityCalc model =
    if model.thrusting
        then ( accel * (fst <| thrustVelocity model)
             , accel * (snd <| thrustVelocity model)
             )
        else (max 0 ((fst model.vel) - decay), max 0 ((snd model.vel) - decay))

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
    }

init : Model
init =
    { pos = (-250,-250)
    , vel = (0,0)
    , target = (0,0)
    , viewport = (windowW, windowH)
    , thrusting = False
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
        halfpi = pi / 2
        nhalfpi = -pi / 2
    in 
        if  | angle > 0 && angle < halfpi ->
                (angle / halfpi, 1 - (angle/halfpi))
            | angle > halfpi ->
                ((angle - halfpi) / halfpi, 1 - ((angle - halfpi) / halfpi))
            | angle < 0 && angle > nhalfpi ->
                (angle / halfpi, 1 - (angle / halfpi))
            | angle < nhalfpi ->
                ((angle + halfpi) / halfpi, 1 - ((angle + halfpi) / halfpi))
            
