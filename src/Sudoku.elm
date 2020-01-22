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


solve : Board -> Board
solve b =
    case emptyCell b of
        Nothing ->
            b

        Just a ->
            case findPossibilites b a of
                Nothing ->
                    False
            
                option2 ->
                    
            



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
