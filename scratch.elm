import Mouse exposing (..)
import Graphics.Element exposing (..)

main : Signal Element
main = Signal.map show Mouse.isDown
