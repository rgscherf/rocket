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
    }

type alias Block =
    { length : Float
    , pos    : Position
    } 
