module SuperTicTacToe exposing (GamePosition, SuperTicTacToe, empty, update, viewBoard)

import Html exposing (Html, div, fieldset, section)
import Html.Attributes exposing (class)
import TicTacToe exposing (Cell(..), Position(..), TicTacToe, WinState(..), viewWinState)


type Turn
    = XTurn
    | OTurn


turnToCell : Turn -> Cell
turnToCell turn =
    case turn of
        XTurn ->
            X

        OTurn ->
            O


type alias SuperTicTacToe =
    { topLeft : TicTacToe
    , topMiddle : TicTacToe
    , topRight : TicTacToe

    ---
    , middleLeft : TicTacToe
    , middleMiddle : TicTacToe
    , middleRight : TicTacToe

    ---
    , bottomLeft : TicTacToe
    , bottomMiddle : TicTacToe
    , bottomRight : TicTacToe

    ---
    , turn : Turn
    , winState : WinState
    , lastMove : Maybe GamePosition
    }


type alias GamePosition =
    { game : Position
    , cell : Position
    }


empty : SuperTicTacToe
empty =
    { topLeft = TicTacToe.empty
    , topMiddle = TicTacToe.empty
    , topRight = TicTacToe.empty

    ---
    , middleLeft = TicTacToe.empty
    , middleMiddle = TicTacToe.empty
    , middleRight = TicTacToe.empty

    ---
    , bottomLeft = TicTacToe.empty
    , bottomMiddle = TicTacToe.empty
    , bottomRight = TicTacToe.empty

    ---
    , turn = XTurn
    , winState = InProgress
    , lastMove = Nothing
    }


set : Cell -> GamePosition -> SuperTicTacToe -> SuperTicTacToe
set cell gamePos board =
    case gamePos.game of
        TopLeft ->
            { board | topLeft = TicTacToe.update cell gamePos.cell board.topLeft }

        TopMiddle ->
            { board | topMiddle = TicTacToe.update cell gamePos.cell board.topMiddle }

        TopRight ->
            { board | topRight = TicTacToe.update cell gamePos.cell board.topRight }

        MiddleLeft ->
            { board | middleLeft = TicTacToe.update cell gamePos.cell board.middleLeft }

        MiddleMiddle ->
            { board | middleMiddle = TicTacToe.update cell gamePos.cell board.middleMiddle }

        MiddleRight ->
            { board | middleRight = TicTacToe.update cell gamePos.cell board.middleRight }

        BottomLeft ->
            { board | bottomLeft = TicTacToe.update cell gamePos.cell board.bottomLeft }

        BottomMiddle ->
            { board | bottomMiddle = TicTacToe.update cell gamePos.cell board.bottomMiddle }

        BottomRight ->
            { board | bottomRight = TicTacToe.update cell gamePos.cell board.bottomRight }


update : GamePosition -> SuperTicTacToe -> SuperTicTacToe
update pos board =
    board
        |> set (turnToCell board.turn) pos
        |> changeTurn
        |> (\b -> { b | lastMove = Just pos })
        |> checkWin


changeTurn : SuperTicTacToe -> SuperTicTacToe
changeTurn board =
    { board
        | turn =
            case board.turn of
                XTurn ->
                    OTurn

                OTurn ->
                    XTurn
    }


boardToNum : TicTacToe -> Int
boardToNum b =
    case b.winState of
        XWinner ->
            1

        OWinner ->
            1

        Draw ->
            1

        InProgress ->
            0


checkDraw : SuperTicTacToe -> Bool
checkDraw board =
    9
        == boardToNum board.topLeft
        + boardToNum board.topMiddle
        + boardToNum board.topRight
        + boardToNum board.middleLeft
        + boardToNum board.middleMiddle
        + boardToNum board.middleRight
        + boardToNum board.bottomLeft
        + boardToNum board.bottomMiddle
        + boardToNum board.bottomRight


checkWin : SuperTicTacToe -> SuperTicTacToe
checkWin board =
    let
        topRow =
            (board.topLeft.winState == board.topMiddle.winState) && (board.topMiddle.winState == board.topRight.winState)

        middleRow =
            (board.middleLeft.winState == board.middleMiddle.winState) && (board.middleMiddle.winState == board.middleRight.winState)

        bottomRow =
            (board.bottomLeft.winState == board.bottomMiddle.winState) && (board.bottomMiddle.winState == board.bottomRight.winState)

        leftCol =
            (board.topLeft.winState == board.middleLeft.winState) && (board.middleLeft.winState == board.bottomLeft.winState)

        middleCol =
            (board.topMiddle.winState == board.middleMiddle.winState) && (board.middleMiddle.winState == board.bottomMiddle.winState)

        rightCol =
            (board.topRight.winState == board.middleRight.winState) && (board.middleRight.winState == board.bottomRight.winState)

        leftToRight =
            (board.topLeft.winState == board.middleMiddle.winState) && (board.middleMiddle.winState == board.bottomRight.winState)

        rightToleft =
            (board.topRight.winState == board.middleMiddle.winState) && (board.middleMiddle.winState == board.bottomLeft.winState)
    in
    { board
        | winState =
            if topRow && board.topLeft.winState == XWinner then
                XWinner

            else if topRow && board.topLeft.winState == OWinner then
                OWinner

            else if middleRow && board.middleLeft.winState == XWinner then
                XWinner

            else if middleRow && board.middleLeft.winState == OWinner then
                OWinner

            else if bottomRow && board.bottomLeft.winState == XWinner then
                XWinner

            else if bottomRow && board.bottomLeft.winState == OWinner then
                OWinner

            else if leftCol && board.topLeft.winState == XWinner then
                XWinner

            else if leftCol && board.topLeft.winState == OWinner then
                OWinner

            else if middleCol && board.topMiddle.winState == XWinner then
                XWinner

            else if middleCol && board.topMiddle.winState == OWinner then
                OWinner

            else if rightCol && board.topRight.winState == XWinner then
                XWinner

            else if rightCol && board.topRight.winState == OWinner then
                OWinner

            else if leftToRight && board.topLeft.winState == XWinner then
                XWinner

            else if leftToRight && board.topLeft.winState == OWinner then
                OWinner

            else if rightToleft && board.topRight.winState == XWinner then
                XWinner

            else if rightToleft && board.topRight.winState == OWinner then
                OWinner

            else if checkDraw board then
                Draw

            else
                InProgress
    }


gameEnded : SuperTicTacToe -> Bool
gameEnded board =
    board.winState /= InProgress


getWinState : Position -> SuperTicTacToe -> WinState
getWinState pos board =
    case pos of
        TopLeft ->
            board.topLeft.winState

        TopMiddle ->
            board.topMiddle.winState

        TopRight ->
            board.topRight.winState

        MiddleLeft ->
            board.middleLeft.winState

        MiddleMiddle ->
            board.middleMiddle.winState

        MiddleRight ->
            board.middleRight.winState

        BottomLeft ->
            board.bottomLeft.winState

        BottomMiddle ->
            board.bottomMiddle.winState

        BottomRight ->
            board.bottomRight.winState


viewBoard : SuperTicTacToe -> (GamePosition -> msg) -> Html msg
viewBoard board msg =
    let
        on game pos =
            msg (GamePosition game pos)

        isLastMove pos =
            board.lastMove
                |> Maybe.map (\m -> m.cell == pos)
                |> Maybe.withDefault True

        gameHasEnded =
            gameEnded board

        gameAtLastPositionEnded =
            board.lastMove
                |> Maybe.map (\m -> getWinState m.cell board)
                |> Maybe.map (\winState -> winState /= InProgress)
                |> Maybe.withDefault True

        enabled pos =
            (isLastMove pos && not gameHasEnded) || gameAtLastPositionEnded
    in
    section [ class "game" ]
        [ fieldset [ class "board" ]
            [ TicTacToe.viewBoard board.topLeft (on TopLeft) (enabled TopLeft)
            , TicTacToe.viewBoard board.topMiddle (on TopMiddle) (enabled TopMiddle)
            , TicTacToe.viewBoard board.topRight (on TopRight) (enabled TopRight)

            ---
            , TicTacToe.viewBoard board.middleLeft (on MiddleLeft) (enabled MiddleLeft)
            , TicTacToe.viewBoard board.middleMiddle (on MiddleMiddle) (enabled MiddleMiddle)
            , TicTacToe.viewBoard board.middleRight (on MiddleRight) (enabled MiddleRight)

            ---
            , TicTacToe.viewBoard board.bottomLeft (on BottomLeft) (enabled BottomLeft)
            , TicTacToe.viewBoard board.bottomMiddle (on BottomMiddle) (enabled BottomMiddle)
            , TicTacToe.viewBoard board.bottomRight (on BottomRight) (enabled BottomRight)
            ]
        , viewWinState board.winState
        ]
