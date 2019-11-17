module Board exposing
    ( Board
    , PlaceResult(..)
    , getNumColumns
    , getNumRows
    , init
    , nextAvailableSlot
    , place
    , view
    )

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Types.Color as Color exposing (Color)


type Board
    = Board
        { columns : Array (Array Color)
        , maxItemsPerCol : Int
        }


type PlaceResult
    = -- TODO: think of a better name
      Place (Result String Board)
    | ColumnAlreadyFull -- Maybe just prevent this?
    | WinningMove
    | BoardIsFull


init : { columns : Int, rows : Int } -> Board
init { columns, rows } =
    Board <|
        { columns = Array.repeat columns Array.empty
        , maxItemsPerCol = rows
        }


getNumRows : Board -> Int
getNumRows (Board board) =
    board.maxItemsPerCol


getNumColumns : Board -> Int
getNumColumns (Board board) =
    board.columns |> Array.length


nextAvailableSlot : { column : Int } -> Board -> Maybe Int
nextAvailableSlot { column } (Board board) =
    board.columns
        |> Array.get column
        |> Maybe.map Array.length
        |> Maybe.andThen
            (\length ->
                if length >= board.maxItemsPerCol then
                    Nothing

                else
                    Just length
            )


place : Color -> { column : Int } -> Board -> PlaceResult
place colorBeingPlaced column ((Board board) as oldBoard) =
    let
        updatedBoardResult =
            board.columns
                |> Array.get column.column
                |> (\c ->
                        case c of
                            Nothing ->
                                Result.Err "Attempted to insert into invalid column"

                            Just col ->
                                { board
                                    | columns =
                                        Array.set column.column
                                            (Array.push colorBeingPlaced col)
                                            board.columns
                                }
                                    |> Board
                                    |> Result.Ok
                   )
    in
    case updatedBoardResult of
        Result.Ok updatedBoard ->
            if isWinningMove colorBeingPlaced column updatedBoard then
                WinningMove

            else if isBoardFull updatedBoard then
                BoardIsFull

            else if nextAvailableSlot column oldBoard == Nothing then
                ColumnAlreadyFull

            else
                Place <| Result.Ok updatedBoard

        Result.Err errStr ->
            Place <| Result.Err errStr


const_NUM_TO_CONNECT : Int
const_NUM_TO_CONNECT =
    4


type DiagonalSlope
    = DiagonalSlopeLeft
    | DiagonalSlopeRight


isWinningMove : Color -> { column : Int } -> Board -> Bool
isWinningMove colorBeingPlaced ({ column } as c) ((Board board) as b) =
    let
        target =
            List.repeat const_NUM_TO_CONNECT (Just colorBeingPlaced)

        containsTarget list =
            -- If the target is the suffix `isInfixOf` seems to return False
            List.Extra.isInfixOf target list || List.Extra.isSuffixOf target list

        -- Check from the where we are dropping one in and the
        -- four below it.
        checkVertical slot =
            case Array.get column board.columns of
                Just col ->
                    let
                        itemsBelowJustPlaced =
                            List.map
                                (\i -> Array.get (slot - i) col)
                                (List.range 0 (const_NUM_TO_CONNECT - 1))
                    in
                    target == itemsBelowJustPlaced

                Nothing ->
                    False

        checkHorizontal slot =
            let
                adjacentItems =
                    List.map
                        (\i -> Array.get i board.columns |> Maybe.andThen (Array.get slot))
                        (List.range
                            (Basics.max 0 <| column - const_NUM_TO_CONNECT - 1)
                            (Basics.min (getNumColumns b - 1) <| column + const_NUM_TO_CONNECT - 1)
                        )
            in
            containsTarget adjacentItems

        checkDiagonals slot =
            let
                adjacentItemsSlope slopeDirection =
                    let
                        slopeDirectionAdjustment =
                            case slopeDirection of
                                DiagonalSlopeLeft ->
                                    (-)

                                DiagonalSlopeRight ->
                                    (+)
                    in
                    List.map
                        (\i ->
                            Array.get (column + i) board.columns
                                |> Maybe.andThen (Array.get (slopeDirectionAdjustment slot i))
                        )
                        (List.range
                            (negate const_NUM_TO_CONNECT - 1)
                            (const_NUM_TO_CONNECT - 1)
                        )

                adjacentItemsSlopeRight =
                    adjacentItemsSlope DiagonalSlopeRight

                adjacentItemsSlopeLeft =
                    adjacentItemsSlope DiagonalSlopeLeft
            in
            containsTarget adjacentItemsSlopeRight || containsTarget adjacentItemsSlopeLeft
    in
    case nextAvailableSlot c b of
        Nothing ->
            False

        Just nextSlot ->
            let
                slotItemWasLastPlacedIn =
                    nextSlot - 1
            in
            checkVertical slotItemWasLastPlacedIn
                || checkHorizontal slotItemWasLastPlacedIn
                || checkDiagonals slotItemWasLastPlacedIn


isFilled : { column : Int } -> Board -> Bool
isFilled { column } board =
    nextAvailableSlot { column = column } board == Nothing


isBoardFull : Board -> Bool
isBoardFull (Board board) =
    board.columns
        |> Array.filter (\col -> Array.length col < board.maxItemsPerCol)
        |> Array.isEmpty


view : ({ column : Int } -> msg) -> Board -> Html msg
view dropItemIntoColumnMsg ((Board board) as b) =
    let
        boxWidthPx =
            100

        marginPx =
            20

        gridCss =
            List.map (\( k, v ) -> HA.style k v)
                [ ( "display", "grid" )
                , ( "grid-gap", String.fromInt marginPx ++ "px" )
                , ( "grid-template-columns"
                  , String.join ""
                        [ "repeat("
                        , String.fromInt (getNumColumns b)
                        , ", "
                        , String.fromInt boxWidthPx
                        , "px)"
                        ]
                  )
                , ( "align-content", "center" )
                , ( "justify-content", "center" )
                , ( "padding-top", String.fromInt marginPx ++ "px" )
                , ( "padding-bottom", String.fromInt marginPx ++ "px" )
                , ( "background-color", Color.toHexString Color.Black )
                ]

        boxCss =
            List.map (\( k, v ) -> HA.style k v)
                [ ( "height", "100px" )
                , ( "margin-top", "20px" )
                , ( "background-color", "white" )
                , ( "border-radius", "100%" )
                ]

        viewBox maybeColor { isPreview } =
            Html.div
                (List.concat
                    [ boxCss
                    , if isPreview then
                        [ HA.class "preview" ]

                      else
                        []

                    -- Conditionally apply this since we want to have the on hover
                    , case maybeColor of
                        Nothing ->
                            []

                        Just color ->
                            [ HA.style "background-color" <| Color.toHexString color
                            , HA.style "box-shadow" "1px 2px 4px rgba(0, 0, 0, .5)"
                            ]
                    ]
                )
                []
    in
    Html.div gridCss <|
        List.indexedMap
            (\colIndex col ->
                Html.div
                    (List.concat
                        [ [ HA.class "column" ]
                        , if Array.length col == board.maxItemsPerCol then
                            [ HA.style "cursor" "not-allowed" ]

                          else
                            [ HE.onClick (dropItemIntoColumnMsg { column = colIndex })
                            , HA.style "cursor" "pointer"
                            ]
                        ]
                    )
                    (List.map
                        (\rowIndex ->
                            viewBox
                                (Array.get rowIndex col)
                                { isPreview =
                                    case nextAvailableSlot { column = colIndex } b of
                                        Just availiableSlotIndex ->
                                            rowIndex == availiableSlotIndex

                                        Nothing ->
                                            False
                                }
                        )
                        (List.range 0 (board.maxItemsPerCol - 1) |> List.reverse)
                    )
            )
            (board.columns |> Array.toList)
