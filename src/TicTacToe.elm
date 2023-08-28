module TicTacToe exposing (Cell(..), Position(..), TicTacToe, WinState(..), empty, update, viewBoard, viewWinState)

import Html exposing (Html, button, div, fieldset, h1, section, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)


type Cell
    = Empty
    | X
    | O


type WinState
    = XWinner
    | OWinner
    | Draw
    | InProgress


type Position
    = TopLeft
    | TopMiddle
    | TopRight
      ---
    | MiddleLeft
    | MiddleMiddle
    | MiddleRight
      ---
    | BottomLeft
    | BottomMiddle
    | BottomRight


type alias TicTacToe =
    { topLeft : Cell
    , topMiddle : Cell
    , topRight : Cell

    ---
    , middleLeft : Cell
    , middleMiddle : Cell
    , middleRight : Cell

    ---
    , bottomLeft : Cell
    , bottomMiddle : Cell
    , bottomRight : Cell

    ---
    , winState : WinState
    }


empty : TicTacToe
empty =
    { topLeft = Empty
    , topMiddle = Empty
    , topRight = Empty

    ---
    , middleLeft = Empty
    , middleMiddle = Empty
    , middleRight = Empty

    ---
    , bottomLeft = Empty
    , bottomMiddle = Empty
    , bottomRight = Empty

    ---
    , winState = InProgress
    }


update : Cell -> Position -> TicTacToe -> TicTacToe
update cell pos board =
    if isTaken pos board || gameEnded board then
        board

    else
        case pos of
            TopLeft ->
                { board | topLeft = cell } |> checkWin

            TopMiddle ->
                { board | topMiddle = cell } |> checkWin

            TopRight ->
                { board | topRight = cell } |> checkWin

            MiddleLeft ->
                { board | middleLeft = cell } |> checkWin

            MiddleMiddle ->
                { board | middleMiddle = cell } |> checkWin

            MiddleRight ->
                { board | middleRight = cell } |> checkWin

            BottomLeft ->
                { board | bottomLeft = cell } |> checkWin

            BottomMiddle ->
                { board | bottomMiddle = cell } |> checkWin

            BottomRight ->
                { board | bottomRight = cell } |> checkWin


get : Position -> TicTacToe -> Cell
get pos board =
    case pos of
        TopLeft ->
            board.topLeft

        TopMiddle ->
            board.topMiddle

        TopRight ->
            board.topRight

        MiddleLeft ->
            board.middleLeft

        MiddleMiddle ->
            board.middleMiddle

        MiddleRight ->
            board.middleRight

        BottomLeft ->
            board.bottomLeft

        BottomMiddle ->
            board.bottomMiddle

        BottomRight ->
            board.bottomRight


isTaken : Position -> TicTacToe -> Bool
isTaken pos board =
    get pos board /= Empty


gameEnded : TicTacToe -> Bool
gameEnded board =
    board.winState /= InProgress


cellToNum : Cell -> Int
cellToNum cell =
    case cell of
        X ->
            1

        O ->
            1

        Empty ->
            0


checkDraw : TicTacToe -> Bool
checkDraw board =
    9
        == cellToNum board.topLeft
        + cellToNum board.topMiddle
        + cellToNum board.topRight
        + cellToNum board.middleLeft
        + cellToNum board.middleMiddle
        + cellToNum board.middleRight
        + cellToNum board.bottomLeft
        + cellToNum board.bottomMiddle
        + cellToNum board.bottomRight


checkWin : TicTacToe -> TicTacToe
checkWin board =
    let
        topRow =
            (board.topLeft == board.topMiddle) && (board.topMiddle == board.topRight)

        middleRow =
            (board.middleLeft == board.middleMiddle) && (board.middleMiddle == board.middleRight)

        bottomRow =
            (board.bottomLeft == board.bottomMiddle) && (board.bottomMiddle == board.bottomRight)

        leftCol =
            (board.topLeft == board.middleLeft) && (board.middleLeft == board.bottomLeft)

        middleCol =
            (board.topMiddle == board.middleMiddle) && (board.middleMiddle == board.bottomMiddle)

        rightCol =
            (board.topRight == board.middleRight) && (board.middleRight == board.bottomRight)

        leftToRight =
            (board.topLeft == board.middleMiddle) && (board.middleMiddle == board.bottomRight)

        rightToleft =
            (board.topRight == board.middleMiddle) && (board.middleMiddle == board.bottomLeft)
    in
    { board
        | winState =
            if topRow && board.topLeft == X then
                XWinner

            else if topRow && board.topLeft == O then
                OWinner

            else if middleRow && board.middleLeft == X then
                XWinner

            else if middleRow && board.middleLeft == O then
                OWinner

            else if bottomRow && board.bottomLeft == X then
                XWinner

            else if bottomRow && board.bottomLeft == O then
                OWinner

            else if leftCol && board.topLeft == X then
                XWinner

            else if leftCol && board.topLeft == O then
                OWinner

            else if middleCol && board.topMiddle == X then
                XWinner

            else if middleCol && board.topMiddle == O then
                OWinner

            else if rightCol && board.topRight == X then
                XWinner

            else if rightCol && board.topRight == O then
                OWinner

            else if leftToRight && board.topLeft == X then
                XWinner

            else if leftToRight && board.topLeft == O then
                OWinner

            else if rightToleft && board.topRight == X then
                XWinner

            else if rightToleft && board.topRight == O then
                OWinner

            else if checkDraw board then
                Draw

            else
                InProgress
    }


viewCell : Cell -> msg -> Bool -> Html msg
viewCell cell msg isDisabled =
    let
        cellStr =
            case cell of
                X ->
                    "X"

                O ->
                    "O"

                Empty ->
                    ""
    in
    button
        [ onClick msg
        , class "cell"
        , disabled isDisabled
        ]
        [ text cellStr ]


viewWinState : WinState -> Html msg
viewWinState state =
    let
        winStateStr =
            case state of
                XWinner ->
                    "X"

                OWinner ->
                    "O"

                Draw ->
                    "-"

                InProgress ->
                    ""
    in
    div [ class "board-overlay" ] [ text winStateStr ]


viewBoard : TicTacToe -> (Position -> msg) -> Bool -> Html msg
viewBoard board msg enabled =
    section []
        [ fieldset [ class "board", disabled (not enabled) ]
            [ viewCell board.topLeft (msg TopLeft) (isTaken TopLeft board || gameEnded board)
            , viewCell board.topMiddle (msg TopMiddle) (isTaken TopMiddle board || gameEnded board)
            , viewCell board.topRight (msg TopRight) (isTaken TopRight board || gameEnded board)

            ---
            , viewCell board.middleLeft (msg MiddleLeft) (isTaken MiddleLeft board || gameEnded board)
            , viewCell board.middleMiddle (msg MiddleMiddle) (isTaken MiddleMiddle board || gameEnded board)
            , viewCell board.middleRight (msg MiddleRight) (isTaken MiddleRight board || gameEnded board)

            ---
            , viewCell board.bottomLeft (msg BottomLeft) (isTaken BottomLeft board || gameEnded board)
            , viewCell board.bottomMiddle (msg BottomMiddle) (isTaken BottomMiddle board || gameEnded board)
            , viewCell board.bottomRight (msg BottomRight) (isTaken BottomRight board || gameEnded board)
            ]
        , viewWinState board.winState
        ]
