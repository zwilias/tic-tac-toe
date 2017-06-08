module App exposing (..)

import Board exposing (..)
import Html exposing (Html, beginnerProgram, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import TicTacToe as TTT
    exposing
        ( Cross
        , Either(..)
        , Move(..)
        , Naught
        )


type Game
    = CrossMove (TTT.Game Cross Naught)
    | NaughtMove (TTT.Game Naught Cross)


type FinishedGame
    = CrossOrDraw (TTT.FinishedGame Cross)
    | NaughtOrDraw (TTT.FinishedGame Naught)


type Model
    = Ongoing Game
    | Finished FinishedGame


initialModel : Model
initialModel =
    Ongoing <| CrossMove TTT.init


type Msg
    = Click Position


update : Msg -> Model -> Model
update (Click position) model =
    case model of
        Ongoing game ->
            case game of
                CrossMove game ->
                    move position game NaughtMove CrossOrDraw

                NaughtMove game ->
                    move position game CrossMove NaughtOrDraw

        Finished finishedGame ->
            model


move : Position -> TTT.Game a b -> (TTT.Game b a -> Game) -> (TTT.FinishedGame a -> FinishedGame) -> Model
move position game asGame asFinished =
    case TTT.move position game of
        Left game ->
            Ongoing <| asGame game

        Right winner ->
            Finished <| asFinished winner


board : Game -> Board Move
board either =
    case either of
        NaughtMove game ->
            TTT.board game

        CrossMove game ->
            TTT.board game


view : Model -> Html Msg
view model =
    case model of
        Ongoing game ->
            showBoard True <| board game

        Finished finished ->
            case finished of
                CrossOrDraw (TTT.Winner _ board) ->
                    showDone "Crosses win!" board

                NaughtOrDraw (TTT.Winner _ board) ->
                    showDone "Naughts win!" board

                CrossOrDraw (TTT.Draw board) ->
                    showDone "Draw..." board

                NaughtOrDraw (TTT.Draw board) ->
                    showDone "Draw..." board


showDone : String -> Board Move -> Html Msg
showDone whoWon board =
    div []
        [ showBoard False board
        , Html.h1 [] [ text whoWon ]
        ]


showBoard : Bool -> Board Move -> Html Msg
showBoard withTrigger board =
    let
        showRow : Int -> List Move -> Html Msg
        showRow y cells =
            div [ class "row" ] (List.indexedMap (showCell y) cells)

        cellString : Move -> String
        cellString move =
            case move of
                TTT.CrossMove ->
                    "X"

                TTT.NaughtMove ->
                    "O"

                Empty ->
                    ""

        trigger : Bool -> Int -> Int -> List (Html.Attribute Msg)
        trigger isClickable x y =
            if isClickable && withTrigger then
                [ onClick <| Click { x = x, y = y } ]
            else
                []

        showCell : Int -> Int -> Move -> Html Msg
        showCell y x cell =
            div
                ([ class "cell" ] ++ trigger (cell == Empty) x y)
                [ text <| cellString cell ]
    in
    div [ class "table" ] (List.indexedMap showRow (rows board))


main : Program Never Model Msg
main =
    beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
