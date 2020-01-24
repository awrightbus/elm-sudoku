module Sudoku exposing (..)

import Browser
import Html exposing (Html, table, text, tr)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)



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


strToVal : String -> Maybe Value
strToVal s =
    case s of
        "1" ->
            Just One

        "2" ->
            Just Two

        "3" ->
            Just Three

        "4" ->
            Just Four

        "5" ->
            Just Five

        "6" ->
            Just Six

        "7" ->
            Just Seven

        "8" ->
            Just Eight

        "9" ->
            Just Nine

        _ ->
            Nothing


values : List (Maybe Value)
values =
    [ Just One, Just Two, Just Three, Just Four, Just Five, Just Six, Just Seven, Just Eight, Just Nine ]


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Cell =
    ( Pos, Maybe Value )


type alias Board =
    List Cell


exampleBoard : Board
exampleBoard =
    [ ( { x = 1, y = 1 }, Nothing )
    , ( { x = 1, y = 2 }, Nothing )
    , ( { x = 1, y = 3 }, Nothing )
    , ( { x = 1, y = 4 }, Nothing )
    , ( { x = 1, y = 5 }, Nothing )
    , ( { x = 1, y = 6 }, Just Six )
    , ( { x = 1, y = 7 }, Just Three )
    , ( { x = 1, y = 8 }, Just Eight )
    , ( { x = 1, y = 9 }, Just Two )
    , ( { x = 2, y = 1 }, Nothing )
    , ( { x = 2, y = 2 }, Just Two )
    , ( { x = 2, y = 3 }, Nothing )
    , ( { x = 2, y = 4 }, Nothing )
    , ( { x = 2, y = 5 }, Nothing )
    , ( { x = 2, y = 6 }, Nothing )
    , ( { x = 2, y = 7 }, Nothing )
    , ( { x = 2, y = 8 }, Nothing )
    , ( { x = 2, y = 9 }, Nothing )
    , ( { x = 3, y = 1 }, Just Four )
    , ( { x = 3, y = 2 }, Nothing )
    , ( { x = 3, y = 3 }, Just One )
    , ( { x = 3, y = 4 }, Just Two )
    , ( { x = 3, y = 5 }, Nothing )
    , ( { x = 3, y = 6 }, Nothing )
    , ( { x = 3, y = 7 }, Nothing )
    , ( { x = 3, y = 8 }, Just Five )
    , ( { x = 3, y = 9 }, Just Seven )
    , ( { x = 4, y = 1 }, Just Three )
    , ( { x = 4, y = 2 }, Just Four )
    , ( { x = 4, y = 3 }, Nothing )
    , ( { x = 4, y = 4 }, Just Seven )
    , ( { x = 4, y = 5 }, Just Five )
    , ( { x = 4, y = 6 }, Nothing )
    , ( { x = 4, y = 7 }, Just Six )
    , ( { x = 4, y = 8 }, Nothing )
    , ( { x = 4, y = 9 }, Nothing )
    , ( { x = 5, y = 1 }, Just Five )
    , ( { x = 5, y = 2 }, Nothing )
    , ( { x = 5, y = 3 }, Nothing )
    , ( { x = 5, y = 4 }, Just Four )
    , ( { x = 5, y = 5 }, Nothing )
    , ( { x = 5, y = 6 }, Just One )
    , ( { x = 5, y = 7 }, Nothing )
    , ( { x = 5, y = 8 }, Nothing )
    , ( { x = 5, y = 9 }, Just Eight )
    , ( { x = 6, y = 1 }, Nothing )
    , ( { x = 6, y = 2 }, Nothing )
    , ( { x = 6, y = 3 }, Just Seven )
    , ( { x = 6, y = 4 }, Nothing )
    , ( { x = 6, y = 5 }, Just Eight )
    , ( { x = 6, y = 6 }, Just Three )
    , ( { x = 6, y = 7 }, Nothing )
    , ( { x = 6, y = 8 }, Just Four )
    , ( { x = 6, y = 9 }, Just Five )
    , ( { x = 7, y = 1 }, Just Two )
    , ( { x = 7, y = 2 }, Just Three )
    , ( { x = 7, y = 3 }, Nothing )
    , ( { x = 7, y = 4 }, Nothing )
    , ( { x = 7, y = 5 }, Nothing )
    , ( { x = 7, y = 6 }, Just Four )
    , ( { x = 7, y = 7 }, Just Five )
    , ( { x = 7, y = 8 }, Nothing )
    , ( { x = 7, y = 9 }, Just One )
    , ( { x = 8, y = 1 }, Nothing )
    , ( { x = 8, y = 2 }, Nothing )
    , ( { x = 8, y = 3 }, Nothing )
    , ( { x = 8, y = 4 }, Nothing )
    , ( { x = 8, y = 5 }, Nothing )
    , ( { x = 8, y = 6 }, Nothing )
    , ( { x = 8, y = 7 }, Nothing )
    , ( { x = 8, y = 8 }, Just Seven )
    , ( { x = 8, y = 9 }, Nothing )
    , ( { x = 9, y = 1 }, Just Nine )
    , ( { x = 9, y = 2 }, Just Seven )
    , ( { x = 9, y = 3 }, Just Four )
    , ( { x = 9, y = 4 }, Just Six )
    , ( { x = 9, y = 5 }, Nothing )
    , ( { x = 9, y = 6 }, Nothing )
    , ( { x = 9, y = 7 }, Nothing )
    , ( { x = 9, y = 8 }, Nothing )
    , ( { x = 9, y = 9 }, Nothing )
    ]


solveBoard : Board -> Board
solveBoard b =
    let
        allEmpty =
            emptyCells b

        inferred =
            List.filter
                (\x -> List.length (findPossibilities x b) == 1)
                allEmpty

        valuesT =
            List.concatMap (\cell -> findPossibilities cell b) inferred

        needValue =
            List.map (\cell -> updateCell b cell) inferred

        boards =
            List.map2 (\fn value -> fn value) needValue valuesT
    in
    case boards of
        [] ->
            b

        x :: _ ->
            solveBoard x


giveHint : Board -> Board
giveHint b =
    let
        allEmpty =
            emptyCells b

        inferred =
            List.filter
                (\x -> List.length (findPossibilities x b) == 1)
                allEmpty

        valuesT =
            List.concatMap (\cell -> findPossibilities cell b) inferred

        needValue =
            List.map (\cell -> updateCell b cell) inferred

        boards =
            List.map2 (\fn value -> fn value) needValue valuesT
    in
    case List.head boards of
        Nothing ->
            b

        Just board ->
            board


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
            List.map (\idy -> List.filter (\( pos, _ ) -> pos.y == idy) b) numRow
    in
    rowCells


emptyCell : Board -> Maybe Cell
emptyCell b =
    List.reverse (List.filter (\( _, val ) -> val == Nothing) b) |> List.head


emptyCells : Board -> List Cell
emptyCells b =
    List.filter (\( _, val ) -> val == Nothing) b


findPossibilities : Cell -> Board -> List (Maybe Value)
findPossibilities c b =
    let
        row =
            inRow c b

        col =
            inCol c b

        block =
            inBlock c b

        answer =
            values
                |> filterOut row
                |> filterOut col
                |> filterOut block
    in
    answer


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
            ""


showCell : Cell -> Html Msg
showCell ( pos, val ) =
    Html.td [ class "cell" ]
        [ Html.input [ Html.Attributes.value (showValue val), Html.Events.onInput (EditedCell ( pos, val )) ] []
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
    { sboard : Board }


init : Model
init =
    { sboard = exampleBoard
    }


view : Model -> Html Msg
view model =
    Html.div
        [ class "center" ]
        [ Html.h1
            [ class "header" ]
            [ Html.text "Elm Sudoku Solver" ]
        , showBoard model.sboard
        , Html.button [ class "button", Html.Events.onClick ClickedSolved ] [ Html.text "Solve It" ]
        , Html.button [ class "button", Html.Events.onClick GiveHint ] [ Html.text "Give Hint" ]
        , Html.button [ class "button", Html.Events.onClick ClearBoard ] [ Html.text "Clear" ]
        ]


type Msg
    = EditedCell Cell String
    | ClickedSolved
    | ClearBoard
    | GiveHint


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditedCell c s ->
            { model | sboard = updateCell model.sboard c (strToVal s) }

        ClickedSolved ->
            { model | sboard = solveBoard model.sboard }

        ClearBoard ->
            { model | sboard = emptyBoard }

        GiveHint ->
            { model | sboard = giveHint model.sboard }


updateCell : Board -> Cell -> Maybe Value -> Board
updateCell b ( p, _ ) value =
    List.map
        (\( pos, val ) ->
            if pos == p then
                ( pos, value )

            else
                ( pos, val )
        )
        b
