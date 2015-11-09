import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Mouse
import Keyboard exposing (..)
import Time
-- TODO: elm-package install johnpmayer/elm-linear-algebra -y
-- in order to get Math
import Math.Vector2 exposing (vec2)
import Collision2D exposing (..)

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

playerSize : Float
playerSize = 20


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
    let newPos = ( fst model.pos + fst model.vel
                 , snd model.pos + snd model.vel
                 )
    in
        { model
        | pos <- newPos
        , blocks <- List.map 
            (collideBlock model.playerSize model.angle newPos model.vel) 
            model.blocks
        }

collideBlock : Float -> Float -> (Float,Float) -> (Float,Float) -> Block -> Block
collideBlock length angle (x,y) (vx,vy) b =
    let tripoints = triPoints length angle (x,y)
        rectpoints = rectPoints block.length block.pos
    in if any ((List.map (flip isInside (fromVectors triPoints)) rectPoints)
              ++ (List.map (flip isInside (fromVectors rectPoints)) triPoints))
       then {b | color <- red}
       else b

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
    , length : Float
    , blocks : List Block
    }

init : Model
init =
    { pos = (0,0)
    , vel = (0,0)
    , angle = 0
    , viewport = (windowW, windowH)
    , length = playerSize
    , blocks = [ Block 20 (50,50) purple
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
                |> filled purple
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

-- COLLISION CALCULATIONS
-- collision library represents shapes as lists of vertices
-- so we need to be able to convert our Forms to that.

-- for a circle of given radius, find point along the circle
-- at a given angle
rotatedPoint : Float -> (Float,Float) -> Float -> Vec2
rotatedPoint radius (x,y) angle =
    let newx = (+) x <| cos angle * radius
        newy = (+) y <| sin angle * radius
    in vec2 newx newy

-- TRIANGLE FUNCTIONS
-- get radius of triangle
triRadius : Float -> Float
triRadius length = flip (/) 2 <| sqrt ((length ^ 2) + ((length/2) ^ 2))

-- list constituent vertices of rectangle
-- given a triangle's side length and location
-- give back a list of locations of its points
triPoints : Float -> Float -> (Float, Float) -> List Vec2
triPoints length angle (x,y) =
    let radius = triRadius length
    in List.map (rotatedPoint radius (x,y)) 
        [angle, angle + (pi * 2/3), angle + (pi * 4/3)] 

-- RECT FUNCTIONS
-- get radius of rectangle
rectRadius : Float -> Float
rectRadius length = 
    let half = length / 2
    in sqrt <| 2 * (half^2)

-- list constituent vertices of rectangle
rectPoints : Float -> (Float,Float) -> List Vec2
rectPoints length (x,y) =
    let radius = rectRadius length
    in List.map (rotatedPoint radius (x,y))
        [0, pi/2, pi, -pi/2]
