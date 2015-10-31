import Mouse exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Html exposing (..)

main : Signal Html
main = Signal.map view (Signal.map
                        (\(x,y) -> (toFloat x, toFloat y))
                        Mouse.position
                       )


view : (Float, Float) -> Html
view (x,y) =
    div
        []
        [ fromElement (show x)
        , fromElement (show y)
        , fromElement (show (atan2 x y))
        ]
