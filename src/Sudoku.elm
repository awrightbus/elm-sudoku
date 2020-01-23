module Sudoku exposing (..)

import Browser
import Html exposing (Html, table, text, tr)
import Html.Attributes exposing (class, value)



--Sudoku Structure Logic


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


values : List (Maybe Value)
values =
    [ Just One, Just Two, Just Three, Just Four, Just Five, Just Six, Just Seven, Just Eight, Just Nine ]


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Cell =
    ( Pos, Maybe Value )



--Block is a row/cell/block of size 9


type alias Block =
    List Cell


type alias Board =
    List Cell



-- def solve(bo):
--     find = find_empty(bo)
--     if not find:
--         return True
--     else:
--         row, col = find
--     for i in range(1,10):
--         if valid(bo, i, (row, col)):
--             bo[row][col] = i
--             if solve(bo):
--                 return True
--             bo[row][col] = 0
--     return False


size : Int
size =
    9


emptyBoard : Board
emptyBoard =
    List.concatMap (\idx -> List.map (\idy -> ( { x = idx, y = idy }, Nothing )) (List.range 1 size)) (List.range 1 size)


rows b =
    let
        numRow =
            List.range 1 9

        rowCells =
            List.map (\idx -> List.filter (\( pos, _ ) -> pos.x == idx) b) numRow
    in
    rowCells


cols b =
    let
        numRow =
            List.range 1 9

        rowCells =
            List.concatMap (\idy -> List.map (\( pos, _ ) -> pos.y == idy) b) numRow
    in
    rowCells


emptyCell : Board -> Maybe Cell
emptyCell b =
    List.head (List.filter (\( _, val ) -> val == Nothing) b)


changeCellValue : Board -> Cell -> Value -> Board
changeCellValue b ( pos, _ ) v =
    List.map
        (\( p, val ) ->
            if pos == p then
                ( p, Just v )

            else
                ( p, val )
        )
        b


findPossibilities : Cell -> Board -> List (Maybe Value)
findPossibilities c b =
    let
        row =
            inRow c b

        col =
            inCol c b

        block =
            inBlock c b
    in
    values
        |> filterOut row
        |> filterOut col
        |> filterOut block


filterOut : (a -> Bool) -> List a -> List a
filterOut f xs =
    List.filter (not << f) xs


inRow : Cell -> Board -> Maybe Value -> Bool
inRow ( pos, _ ) b v =
    let
        row =
            List.filter (\( p, _ ) -> p.x == pos.x) b

        rowValues =
            List.map (\( _, val ) -> val) row
    in
    List.member v rowValues


inCol : Cell -> Board -> Maybe Value -> Bool
inCol ( pos, _ ) b v =
    let
        col =
            List.filter (\( p, _ ) -> p.y == pos.y) b

        colValues =
            List.map (\( _, val ) -> val) col
    in
    List.member v colValues


inBlock : Cell -> Board -> Maybe Value -> Bool
inBlock ( pos, _ ) b v =
    let
        getBlock =
            \x -> (x - 1) // 3

        block =
            List.filter (\( p, _ ) -> getBlock p.x == getBlock pos.x && getBlock p.y == getBlock pos.y) b

        blockValues =
            List.map (\( _, val ) -> val) block
    in
    List.member v blockValues



--Sudoku Viewing Logic


showValue : Maybe Value -> String
showValue v =
    case v of
        Just One ->
            "1"

        Just Two ->
            "2"

        Just Three ->
            "3"

        Just Four ->
            "4"

        Just Five ->
            "5"

        Just Six ->
            "6"

        Just Seven ->
            "7"

        Just Eight ->
            "8"

        Just Nine ->
            "9"

        Nothing ->
            " "


showCell : Cell -> Html Msg
showCell ( _, val ) =
    Html.td [ class "cell" ]
        [ Html.input [ Html.Attributes.value (showValue val) ] []
        ]


showRow : List Cell -> Html Msg
showRow cs =
    Html.tr [ class "row" ]
        (List.map (\c -> showCell c) cs)


showBoard : Board -> Html Msg
showBoard b =
    Html.div [ class "board" ]
        [ Html.table
            [ class "center" ]
            (List.map (\r -> showRow r) (rows b))
        ]



-- Browser Logic


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { sboard : Board
    }


init : Model
init =
    { sboard = emptyBoard
    }


view : Model -> Html Msg
view model =
    Html.div
        [ class "center" ]
        [ Html.h1
            [ class "header" ]
            [ Html.text "Elm Sudoku Solver" ]
        , showBoard model.sboard
        , Html.button [ class "button" ] [ Html.text "Solve It" ]
        ]


type Msg
    = ClickedCell


update : Msg -> Model -> Model
update _ model =
    model
