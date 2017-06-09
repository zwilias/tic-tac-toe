module TicTacToe
    exposing
        ( Cross
        , FinishedGame(..)
        , Game
        , Move(..)
        , Naught
        , board
        , init
        , move
        )

import Board exposing (Board)
import Either exposing (Either(..))


type Move
    = CrossMove
    | NaughtMove


type Game current next
    = Game
        current
        next
        { board : Board Move
        , move : current -> Move
        , nextMove : next -> Move
        }


type FinishedGame winner
    = Winner winner (Board Move)
    | Draw (Board Move)


type Cross
    = Cross


type Naught
    = Naught


init : Game Cross Naught
init =
    { board = Board.init, move = crossMove, nextMove = naughtMove }
        |> Game Cross Naught


move :
    Board.Cell Board.Free
    -> Game current next
    -> Either (Game next current) (FinishedGame current)
move position game_ =
    let
        game =
            applyMove position game_
    in
    case markWinner game of
        Just done ->
            Right <| done

        Nothing ->
            Left <| stepGame game


board : Game a b -> Board Move
board (Game _ _ { board }) =
    board


allSame : List (Maybe Move) -> Bool
allSame list =
    case list of
        [] ->
            False

        Nothing :: _ ->
            False

        x :: xs ->
            List.all ((==) x) xs


hasComplete : (Board Move -> List (List (Maybe Move))) -> Board Move -> Bool
hasComplete f a =
    f a |> List.any allSame


isFull : Board Move -> Bool
isFull arr =
    Board.toList arr |> List.all ((/=) Nothing)


markWinner : Game current next -> Maybe (FinishedGame current)
markWinner (Game current _ { board }) =
    if
        hasComplete Board.rows board
            || hasComplete Board.columns board
            || hasComplete Board.diagonals board
    then
        Just <| Winner current board
    else if isFull board then
        Just <| Draw board
    else
        Nothing


crossMove : Cross -> Move
crossMove _ =
    CrossMove


naughtMove : Naught -> Move
naughtMove _ =
    NaughtMove


stepGame : Game a b -> Game b a
stepGame (Game a b game) =
    { game | move = game.nextMove, nextMove = game.move }
        |> Game b a


applyMove : Board.Cell Board.Free -> Game a b -> Game a b
applyMove position (Game current next ({ move, board } as game)) =
    let
        updatedBoard =
            Board.set position (move current) board
    in
    { game | board = updatedBoard } |> Game current next
