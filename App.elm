module App exposing (..)

import Board exposing (..)
import Html exposing (Html, beginnerProgram, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import TicTacToe as TTT exposing (Cross, Either(..), FinishedGame, Game, Move(..), Naught)


type alias Game =
    Either (TTT.Game Cross Naught) (TTT.Game Naught Cross)


type alias Model =
    { game : Game
    , winner : Maybe String
    }


initialModel : Model
initialModel =
    { game = Left TTT.init
    , winner = Nothing
    }


type Msg
    = Click Position


update : Msg -> Model -> Model
update (Click position) ({ game } as model) =
    case game of
        Left game ->
            move position game Right model

        Right game ->
            move position game Left model


move : Position -> TTT.Game a b -> (TTT.Game b a -> Game) -> Model -> Model
move position game forModel model =
    case TTT.move position game of
        Left new ->
            { model | game = forModel new }

        Right winner ->
            { model | winner = Just <| toString winner }


board : Game -> Board Move
board either =
    case either of
        Left game ->
            TTT.board game

        Right game ->
            TTT.board game


view : Model -> Html Msg
view model =
    div []
        [ showBoard <| board model.game
        , text <| toString model.winner
        ]


showBoard : Board Move -> Html Msg
showBoard board =
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

        showCell : Int -> Int -> Move -> Html Msg
        showCell y x cell =
            div [ class "cell", onClick <| Click { x = x, y = y } ] [ text <| cellString cell ]
    in
    div [ class "table" ] (List.indexedMap showRow (rows board))


main : Program Never Model Msg
main =
    beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
