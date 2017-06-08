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


row : Int -> Board a -> List a
row row arr =
    List.range 0 2
        |> List.filterMap (\x -> Array.get (toIndex { x = x, y = row }) arr)


column : Int -> Board a -> List a
column col arr =
    List.range 0 2
        |> List.filterMap (\y -> Array.get (toIndex { x = col, y = y }) arr)


diagonal1 : Board a -> List a
diagonal1 arr =
    List.range 0 2
        |> List.filterMap (\i -> Array.get (toIndex { x = i, y = i }) arr)


diagonal2 : Board a -> List a
diagonal2 arr =
    List.range 0 2
        |> List.filterMap (\i -> Array.get (toIndex { x = i, y = 2 - i }) arr)


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
