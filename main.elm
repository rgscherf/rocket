import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window
import Time

-- CONSTANTS

-- window w and h
windowside : Int
windowside = 700

-- canvas scalar
-- change with window size to maintain
-- proper level size...?
scalar : Float
scalar = 0.03

-- acceleration constant
accel : Float
accel = 0.25 * scalar

-- deceleration constant
decay : Float
decay = 0.1 * scalar


-- SIGNALS & ACTIONS

signals : Signal Action
signals = Signal.mergeMany
    [ Signal.map MouseButton Mouse.isDown
    , Signal.map Tick (Time.fps 60)
    ]

type Action
    = MouseButton Bool
    | Tick Float

model : Signal Model
model = Signal.foldp update init signals

main : Signal Element
main = Signal.map render model


-- UPDATE
update : Action -> Model -> Model
update action model =
    case action of
        MouseButton b ->
            { model | thrusting <- b }
        Tick _ ->
            let velocity =
                if model.thrusting
                    then ((fst model.vel) + accel, (snd model.vel) + accel)
                    else (max 0 ((fst model.vel) - decay), max 0 ((snd model.vel) - decay))
            in
                { model
                | pos <- ( fst model.pos + fst velocity
                         , snd model.pos + snd velocity
                         )
                , vel <- velocity
                }


-- MODEL
type alias Model =
    { pos : (Float, Float)
    , vel : (Float, Float)
    , thrusting : Bool
    }

init : Model
init =
    { pos = (-250,-250)
    , vel = (0,0)
    , thrusting = False
    }


-- VIEW
render : Model -> Element
render model =
    collage windowside windowside
        [ ngon 3 20
            |> filled green
            |> move (fst model.pos, snd model.pos)
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
