module RktTypes where

import Math.Vector2 exposing (..)
import Color exposing (..)

type alias Velocity = Vec2
type alias Position = Vec2

type alias Model =
    { pos        : Position
    , vel        : Velocity
    , angle      : Float
    , viewport   : (Int, Int)
    , playerSize : Float
    , blocks     : List Block
    , debug      : Bool
    , trail      : List (Int, Vec2)
    , paused : Bool
    }

type Tile
    = Wall
    | SlowPad
    | ExitPad

type alias Block =
    { length : Float
    , tile   : Tile
    , pos    : Position
    } 

defaultBlock : Float -> Float -> Block
defaultBlock x y =
    let length = 30
        half   = length / 2
    in Block length Wall <| vec2 (x + half) (y - half) 
