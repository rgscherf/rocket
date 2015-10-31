import Mouse exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Html exposing (..)
import Color exposing (..)
import Html.Attributes exposing (..)

clickPos : Signal (Int,Int)
clickPos = Signal.sampleOn Mouse.clicks Mouse.position

view : (Int, Int) -> Html
view (x,y) =
    let relativex = toFloat x - halfx
        relativey = halfy - toFloat y
        collagex = 500
        collagey = 500
        halfx = (collagex / 2.0)
        halfy = (collagey / 2.0)
    in
        div
            []
            [ collage collagex collagey
                [ rect collagex collagey |> filled grey
                -- axes
                , traced defaultLine <| segment (-halfx, 0) (halfx, 0)
                , traced defaultLine <| segment (0, -halfy) (0, halfy)
                -- click location
                , ngon 6 5 |> filled orange |> move (relativex, relativey)
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
main = Signal.map view clickPos
