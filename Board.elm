module Board exposing (Board, Cell, Free, Taken, cell, columns, diagonals, init, rows, set, toList)

import Array exposing (Array)
import Either exposing (Either(..))


type Taken
    = Taken


type Free
    = Free


type alias Board a =
    Array (Maybe a)


type Cell a
    = Cell Position


type alias Position =
    { x : Int, y : Int }


cell : Position -> Board a -> Either (Cell Free) (Cell Taken)
cell pos board =
    let
        cellContent : Maybe a
        cellContent =
            Array.get (toIndex pos) board
                |> Maybe.withDefault Nothing
    in
    case cellContent of
        Nothing ->
            Left (Cell pos)

        Just _ ->
            Right (Cell pos)


set : Cell Free -> a -> Board a -> Board a
set (Cell pos) val board =
    Array.set (toIndex pos) (Just val) board


toIndex : Position -> Int
toIndex { x, y } =
    x + 3 * y


type alias Extractor =
    Int -> Position


extract : (Int -> Position) -> Array a -> Int -> Maybe a
extract with from index =
    Array.get (with index |> toIndex) from


forAll : Extractor -> Array a -> List a
forAll extractor arr =
    List.range 0 2 |> List.filterMap (extract extractor arr)


row : Int -> Board a -> List (Maybe a)
row row =
    forAll (\x -> { x = x, y = row })


column : Int -> Board a -> List (Maybe a)
column col =
    forAll (\y -> { x = col, y = y })


diagonal1 : Board a -> List (Maybe a)
diagonal1 =
    forAll (\i -> { x = i, y = i })


diagonal2 : Board a -> List (Maybe a)
diagonal2 =
    forAll (\i -> { x = i, y = 2 - i })


rows : Board a -> List (List (Maybe a))
rows arr =
    List.range 0 2 |> List.map (flip row arr)


columns : Board a -> List (List (Maybe a))
columns arr =
    List.range 0 2 |> List.map (flip column arr)


diagonals : Board a -> List (List (Maybe a))
diagonals arr =
    [ diagonal1 arr, diagonal2 arr ]


toList : Board a -> List (Maybe a)
toList =
    Array.toList


init : Board a
init =
    Array.initialize 9 (always Nothing)
