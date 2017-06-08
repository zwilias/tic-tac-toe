module Board exposing (..)

import Array exposing (Array)


type alias Board a =
    Array a


type alias Position =
    { x : Int, y : Int }


get : Position -> Board a -> Maybe a
get pos board =
    Array.get (toIndex pos) board


set : Position -> a -> Board a -> Board a
set pos val board =
    Array.set (toIndex pos) val board


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


row : Int -> Board a -> List a
row row =
    forAll (\x -> { x = x, y = row })


column : Int -> Board a -> List a
column col =
    forAll (\y -> { x = col, y = y })


diagonal1 : Board a -> List a
diagonal1 =
    forAll (\i -> { x = i, y = i })


diagonal2 : Board a -> List a
diagonal2 =
    forAll (\i -> { x = i, y = 2 - i })


rows : Board a -> List (List a)
rows arr =
    List.range 0 2 |> List.map (flip row arr)


columns : Board a -> List (List a)
columns arr =
    List.range 0 2 |> List.map (flip column arr)


diagonals : Board a -> List (List a)
diagonals arr =
    [ diagonal1 arr, diagonal2 arr ]


toList : Board a -> List a
toList =
    Array.toList


init : a -> Board a
init v =
    Array.initialize 9 (always v)
