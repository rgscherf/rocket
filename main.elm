import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Mouse
import Time

-- CONSTANTS

-- window w and h
-- windowside : Int
-- windowside = 700

-- canvas scalar
-- change with window size to maintain
-- proper level size...?
scalar : Float
scalar = 0.02

-- acceleration constant
accel : Float
accel = 0.25 * scalar

-- deceleration constant
decay : Float
decay = 0.12 * scalar


-- SIGNALS & ACTIONS

signals : Signal Action
signals = Signal.mergeMany
    [ Signal.map Thrust Mouse.isDown
    , Signal.map Target Mouse.position
    , Signal.map Tick (Time.fps 60)
    ]

type Action
    = Target (Int,Int)
    | Thrust Bool
    | Tick Float

model : Signal Model
model = Signal.foldp update init signals

main : Signal Element
main = Signal.map2 render Window.dimensions model


-- UPDATE
update : Action -> Model -> Model
update action model =
    case action of
        Target (x,y) ->
            { model | target <- (toFloat x, toFloat y) }
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
        then ((fst model.vel) + accel, (snd model.vel) + accel)
        else (max 0 ((fst model.vel) - decay), max 0 ((snd model.vel) - decay))

-- MODEL
type alias Model =
    { pos : (Float, Float)
    , vel : (Float, Float)
    , target : (Float, Float)
    , thrusting : Bool
    }

init : Model
init =
    { pos = (-250,-250)
    , vel = (0,0)
    , target = (0,0)
    , thrusting = False
    }


-- VIEW
render : (Int, Int) -> Model -> Element
render (w,h) model =
    collage w h
        [ ngon 3 10
            |> filled green
            |> move (fst model.pos, snd model.pos)
            -- rotate to last clicked
        ]

-- scene : (Int, Int) -> (Int, Int) -> Element
-- scene (x,y) (w,h) =
--     let (dx, dy) =
--         (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
--     in
--         collage w h
--             [ ngon 3 20
--                 |> filled blue
--                 |> rotate (atan2 dy dx)
--             , ngon 8 5
--                 |> filled orange
--                 |> move (dx, dy)
--             ]
