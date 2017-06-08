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


type alias Game =
    Either (TTT.Game Cross Naught) (TTT.Game Naught Cross)


type alias FinishedGame =
    Either (TTT.FinishedGame Cross) (TTT.FinishedGame Naught)


type alias Model =
    Either Game FinishedGame


initialModel : Model
initialModel =
    Left <| Left TTT.init


type Msg
    = Click Position


update : Msg -> Model -> Model
update (Click position) model =
    case model of
        Left game ->
            case game of
                Left game ->
                    move position game Right Left

                Right game ->
                    move position game Left Right

        Right finishedGame ->
            model


move : Position -> TTT.Game a b -> (TTT.Game b a -> Game) -> (TTT.FinishedGame a -> FinishedGame) -> Model
move position game asGame asFinished =
    case TTT.move position game of
        Left game ->
            Left <| asGame game

        Right winner ->
            Right <| asFinished winner


board : Game -> Board Move
board either =
    case either of
        Left game ->
            TTT.board game

        Right game ->
            TTT.board game


view : Model -> Html Msg
view model =
    case model of
        Left game ->
            showBoard True <| board game

        Right finished ->
            case finished of
                Left (TTT.Winner _ board) ->
                    showDone "Crosses win!" board

                Right (TTT.Winner _ board) ->
                    showDone "Naughts win!" board

                Left (TTT.Draw board) ->
                    showDone "Draw..." board

                Right (TTT.Draw board) ->
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
                CrossMove ->
                    "X"

                NaughtMove ->
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
