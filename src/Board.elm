module Board exposing
    ( Board
    , PlaceResult(..)
    , init
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
      Placed (Result String Board)
      -- | ColumnAlreadyFull -- On click is removed when full
    | WinningMove
    | BoardIsFull


init : { columns : Int, rows : Int } -> Board
init { columns, rows } =
    Board <|
        { columns = Array.repeat columns Array.empty
        , maxItemsPerCol = rows
        }


place : Color -> { columnIndex : Int } -> Board -> PlaceResult
place colorBeingPlaced ({ columnIndex } as ci) ((Board board) as oldBoard) =
    let
        updatedBoardResult =
            board.columns
                |> Array.get columnIndex
                |> (\c ->
                        case c of
                            Nothing ->
                                Result.Err "Attempted to insert into invalid column"

                            Just col ->
                                { board
                                    | columns =
                                        Array.set columnIndex
                                            (Array.push colorBeingPlaced col)
                                            board.columns
                                }
                                    |> Board
                                    |> Result.Ok
                   )
    in
    case updatedBoardResult of
        Result.Ok updatedBoard ->
            if isWinningMove colorBeingPlaced ci updatedBoard then
                WinningMove

            else if isBoardFull updatedBoard then
                BoardIsFull

            else
                Placed <| Result.Ok updatedBoard

        Result.Err errStr ->
            Placed <| Result.Err errStr


getNumRows : Board -> Int
getNumRows (Board board) =
    board.maxItemsPerCol


getNumColumns : Board -> Int
getNumColumns (Board board) =
    board.columns |> Array.length


nextAvailableRowIndex : { columnIndex : Int } -> Board -> Maybe Int
nextAvailableRowIndex { columnIndex } (Board board) =
    board.columns
        |> Array.get columnIndex
        |> Maybe.map Array.length
        |> Maybe.andThen
            (\length ->
                if length >= board.maxItemsPerCol then
                    Nothing

                else
                    Just length
            )


isBoardFull : Board -> Bool
isBoardFull (Board board) =
    board.columns
        |> Array.filter (\col -> Array.length col < board.maxItemsPerCol)
        |> Array.isEmpty


const_NUM_TO_CONNECT : Int
const_NUM_TO_CONNECT =
    4


type DiagonalSlope
    = DiagonalSlopeLeft
    | DiagonalSlopeRight


isWinningMove : Color -> { columnIndex : Int } -> Board -> Bool
isWinningMove colorBeingPlaced ({ columnIndex } as ci) ((Board board) as b) =
    let
        target =
            List.repeat const_NUM_TO_CONNECT (Just colorBeingPlaced)

        containsTarget list =
            -- If the target is the suffix `isInfixOf` seems to return False
            List.Extra.isInfixOf target list || List.Extra.isSuffixOf target list

        -- Check from the where we are dropping one in and the
        -- four below it.
        checkVertical rowIndex =
            case Array.get columnIndex board.columns of
                Just col ->
                    let
                        itemsBelowJustPlaced =
                            List.map
                                (\i -> Array.get (rowIndex - i) col)
                                (List.range 0 (const_NUM_TO_CONNECT - 1))
                    in
                    target == itemsBelowJustPlaced

                Nothing ->
                    False

        checkHorizontal rowIndex =
            let
                adjacentItems =
                    List.map
                        (\i -> Array.get i board.columns |> Maybe.andThen (Array.get rowIndex))
                        (List.range
                            (Basics.max
                                0
                                (columnIndex - const_NUM_TO_CONNECT - 1)
                            )
                            (Basics.min
                                (getNumColumns b - 1)
                                (columnIndex + const_NUM_TO_CONNECT - 1)
                            )
                        )
            in
            containsTarget adjacentItems

        checkDiagonals rowIndex =
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
                            Array.get (columnIndex + i) board.columns
                                |> Maybe.andThen (Array.get (slopeDirectionAdjustment rowIndex i))
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
    case nextAvailableRowIndex ci b of
        Nothing ->
            False

        Just nextRowIndex ->
            let
                rowIndexItemWasLastPlacedIn =
                    nextRowIndex - 1
            in
            checkVertical rowIndexItemWasLastPlacedIn
                || checkHorizontal rowIndexItemWasLastPlacedIn
                || checkDiagonals rowIndexItemWasLastPlacedIn


view : ({ columnIndex : Int } -> msg) -> Board -> Html msg
view dropItemIntoColumnMsg ((Board board) as b) =
    let
        boxWidthVw =
            "6vw"

        marginVw =
            "1vw"

        gridCss =
            List.map (\( k, v ) -> HA.style k v)
                [ ( "display", "grid" )
                , ( "grid-gap", marginVw )
                , ( "grid-template-columns"
                  , String.join ""
                        [ "repeat("
                        , String.fromInt (getNumColumns b)
                        , ", "
                        , boxWidthVw
                        , ")"
                        ]
                  )
                , ( "align-content", "center" )
                , ( "justify-content", "center" )
                , ( "padding-top", marginVw )
                , ( "padding-bottom", marginVw )
                , ( "background-color", Color.toHexString Color.Black )
                ]

        boxCss =
            List.map (\( k, v ) -> HA.style k v)
                [ ( "height", boxWidthVw )
                , ( "margin-top", marginVw )
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

        viewColumn columnIndex columnArr =
            Html.div
                (List.concat
                    [ if Array.length columnArr == board.maxItemsPerCol then
                        [ HA.style "cursor" "not-allowed" ]

                      else
                        [ HE.onClick (dropItemIntoColumnMsg columnIndex)
                        , HA.style "cursor" "pointer"
                        ]
                    ]
                )
                (List.map
                    (\rowIndex ->
                        viewBox
                            (Array.get rowIndex columnArr)
                            { isPreview =
                                case nextAvailableRowIndex columnIndex b of
                                    Just availiableRowIndex ->
                                        rowIndex == availiableRowIndex

                                    Nothing ->
                                        False
                            }
                    )
                    (List.range 0 (board.maxItemsPerCol - 1) |> List.reverse)
                )
    in
    Html.div gridCss <|
        List.indexedMap
            (\i columnArr -> viewColumn { columnIndex = i } columnArr)
            (board.columns |> Array.toList)
