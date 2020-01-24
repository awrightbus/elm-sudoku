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



--Block is a row/cell/block of size 9


type alias Block =
    List Cell


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


inferredCells : Board -> Board
inferredCells b =
    let
        allEmpty =
            emptyCells b

        infered =
            List.filter
                (\x -> List.length (findPossibilities x b) == 1)
                allEmpty

        valuesT =
            List.concatMap (\cell -> findPossibilities cell b) infered

        needValue =
            List.map (\cell -> updateCell b cell) infered

        boards =
            List.map2 (\fn value -> fn value) needValue valuesT
    in
    case boards of
        [] ->
            b

        x :: _ ->
            inferredCells x


solveBoard : Board -> Maybe Board
solveBoard b =
    let
        ( celltofill, solved ) =
            case emptyCell b of
                Nothing ->
                    ( ( { x = 0, y = 0 }, Nothing ), True )

                Just c ->
                    ( c, False )

        possibleBoards =
            case findPossibilities celltofill b of
                [ Nothing ] ->
                    [ emptyBoard ]

                ls ->
                    List.map
                        (\x ->
                            case x of
                                Nothing ->
                                    changeCellValue b celltofill One

                                Just v ->
                                    changeCellValue b celltofill v
                        )
                        ls

        nextStep =
            List.map solveBoard possibleBoards
    in
    if solved then
        Just b

    else
        case List.head nextStep of
            Nothing ->
                Just b

            Just c ->
                c


isSolved : Board -> Bool
isSolved b =
    case emptyCell b of
        Nothing ->
            if hasConflicts b then
                False

            else
                True

        Just c ->
            False



-- findPossibilities
-- map changeValue c b findPossibilities list of boards possible
-- filter allBoards that
-- def solve(bo):
--     find = find_empty(bo) 1.Find Empty Cell
--     if not find:
--         return True
--     else:
--         row, col = find
--     for i in range(1,10): 2. Find all values for that cell
--         if valid(bo, i, (row, col)): 3. that are valid
--             bo[row][col] = i 4.Create boards with those values
--             if solve(bo): recurse
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
            List.map (\idy -> List.filter (\( pos, _ ) -> pos.y == idy) b) numRow
    in
    rowCells


emptyCell : Board -> Maybe Cell
emptyCell b =
    List.reverse (List.filter (\( _, val ) -> val == Nothing) b) |> List.head


emptyCells : Board -> List Cell
emptyCells b =
    List.filter (\( _, val ) -> val == Nothing) b


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

        answer =
            values
                |> filterOut row
                |> filterOut col
                |> filterOut block
                |> filterOut (\x -> x == Nothing)
    in
    answer


hasConflicts : Board -> Bool
hasConflicts b =
    True


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
        , Html.button [ class "button", Html.Events.onClick FillInferred ] [ Html.text "Fill Inferred" ]
        , Html.button [ class "button", Html.Events.onClick ClearBoard ] [ Html.text "Clear" ]
        ]


type Msg
    = EditedCell Cell String
    | ClickedSolved
    | FillInferred
    | ClearBoard


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditedCell c s ->
            { model | sboard = updateCell model.sboard c (strToVal s) }

        ClickedSolved ->
            case solveBoard model.sboard of
                Nothing ->
                    { model | sboard = model.sboard }

                Just newBoard ->
                    { model | sboard = newBoard }

        FillInferred ->
            { model | sboard = inferredCells model.sboard }

        ClearBoard ->
            { model | sboard = emptyBoard }


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
