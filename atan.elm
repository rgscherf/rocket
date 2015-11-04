import Mouse exposing (..)
import Window exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Html exposing (..)
import Color exposing (..)

clickPos : Signal (Int,Int)
clickPos = Signal.sampleOn Mouse.clicks Mouse.position

view : (Int, Int) -> (Int, Int) -> Html
view (w,h) (x,y) =
    let relativex = toFloat x - halfx
        relativey = halfy - toFloat y
        halfx = (toFloat w / 2.0)
        halfy = (toFloat h / 2.0)
    in
        div
            []
            [ collage w h
                [ rect (toFloat w) (toFloat h) |> filled grey
                -- axes
                , traced defaultLine <| segment (-halfx, 0) (halfx, 0)
                , traced defaultLine <| segment (0, -halfy) (0, halfy)
                -- click location
                , ngon 6 5 
                    |> filled orange 
                    |> move (relativex, relativey)
                -- tracing the triangle
                , makeLine <| segment (0,0) (relativex, 0)
                , makeLine <| segment (0,0) (relativex, relativey)
                , makeLine <| segment (relativex, 0) (relativex, relativey)
                -- tracing radius circle
                , (relativex ^ 2) + (relativey ^ 2)
                    |> sqrt
                    |> circle
                    |> outlined (dashed lightGrey)
                    |> move (0,0)
                ]
                |> fromElement
            , fromElement << show <| atan2 relativey relativex
            ]

makeLine : Path -> Form
makeLine p = traced (dashed lightGrey) p

main : Signal Html
main = Signal.map2 view Window.dimensions clickPos 

-- need function for finding atan2 angle
-- for ship rotation.
-- finding mouse position will 
relativeAngle : (Float,Float) -> (Float, Float) -> Float
relativeAngle (shipx, shipy) (mx, my) =
    let relativex = mx - (toFloat windowW / 2.0)
        relativey = (toFloat windowH / 2.0)
    in
        atan2 ( (relativey - shipy) / (relativex - shipx) )
        
