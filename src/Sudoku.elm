module Sudoku exposing (..)


type Value
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Cell =
    ( Pos, Value )


type alias Board =
    List Cell
