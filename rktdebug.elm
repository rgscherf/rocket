module RktDebug where

import Math.Vector2 exposing (..)

drawDebug : Model -> List Form
drawDebug model =
    [ traced (dashed red) <| path 
        ( List.map toTuple 
            (triToPoints model.playerSize model.angle model.pos)
        )
    ]
    ++ (List.concat (List.map debugBlock model.blocks))

debugBlock : Block -> List Form
debugBlock b = 
    let
        left   = (getX b.pos - (b.length /2), getY b.pos)
        right  = (getX b.pos + (b.length /2), getY b.pos)
        top    = (getX b.pos, getY b.pos + (b.length / 2))
        bottom = (getX b.pos, getY b.pos - (b.length / 2))
    in
    [ traced (dashed red)
        <| path (List.map toTuple (rectToPoints b.length b.pos))
    ]
    ++ List.map 
        (\c -> traced (dashed red) <| segment (toTuple b.pos) c) 
        [left, right, top, bottom]
