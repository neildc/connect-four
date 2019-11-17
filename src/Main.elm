module Main exposing (Model, Msg, init, main, update, view)

import Array exposing (Array)
import Board exposing (Board)
import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Time
import Types.Color as Color exposing (Color)
import Types.Player as Player exposing (Player)



---- CONFIG ----
-- TODO user configurable/load through flags?


const_GAME_COLUMNS : Int
const_GAME_COLUMNS =
    7


const_GAME_ROWS : Int
const_GAME_ROWS =
    6


defaultPlayers : List ( String, Color )
defaultPlayers =
    [ ( "Player 1", Color.Blue )
    , ( "Player 2", Color.Red )

    -- , ( "Player 3", Color.Yellow )
    -- , ( "Player 4", Color.Green )
    ]



---- MODEL ----


type Screen
    = StartScreen
    | GameScreen
    | RoundEndScreen { wasTie : Bool }


type alias Model =
    { screen : Screen
    , board : Board
    , players : Array Player
    , activePlayerIndex : Int
    }


cssGridBox =
    List.map (\( k, v ) -> HA.style k v)
        [ ( "display", "grid" )
        , ( "grid-gap", "30px" )
        , ( "align-content", "center" )
        , ( "justify-content", "center" )
        , ( "width", "50%" )
        , ( "margin-left", "auto" )
        , ( "margin-right", "auto" )
        , ( "margin-top", "100px" )
        , ( "padding", "40px" )
        , ( "border", "solid 5px black" )
        , ( "border-radius", "25px" )
        , ( "background-color", "white" )
        ]


getActivePlayer : Model -> Maybe Player
getActivePlayer model =
    Array.get model.activePlayerIndex model.players


setActivePlayerToNext : Model -> Model
setActivePlayerToNext model =
    { model
        | activePlayerIndex =
            Basics.modBy
                (Array.length model.players)
                (model.activePlayerIndex + 1)
    }


init : ( Model, Cmd Msg )
init =
    ( { screen = StartScreen
      , board = Board.init { columns = const_GAME_COLUMNS, rows = const_GAME_ROWS }
      , players =
            Array.fromList <|
                List.map (\( name, color ) -> Player.init name color) defaultPlayers
      , activePlayerIndex = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = PlayerEdited { playerIndex : Int } Player
    | RestartGame
    | PlayerMadeAMove { activePlayer : Player } { column : Int }
    | ReturnToStartScreen
    | TickEverySecond


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerEdited { playerIndex } player ->
            ( { model | players = model.players |> Array.set playerIndex player }
            , Cmd.none
            )

        RestartGame ->
            ( { model
                | screen = GameScreen
                , board = Board.init { columns = const_GAME_COLUMNS, rows = const_GAME_ROWS }
              }
                |> setActivePlayerToNext
            , Cmd.none
            )

        PlayerMadeAMove { activePlayer } column ->
            ( case Board.place activePlayer.color column model.board of
                Board.Place result ->
                    case result of
                        Result.Err errStr ->
                            Debug.todo "Alert?"

                        Result.Ok newBoard ->
                            { model | board = newBoard }
                                |> setActivePlayerToNext

                Board.ColumnAlreadyFull ->
                    model |> Debug.log "TODO handle column full alert or something"

                Board.WinningMove ->
                    let
                        maybeWinner =
                            getActivePlayer model
                    in
                    case maybeWinner of
                        Just winner ->
                            { model
                                | screen = RoundEndScreen { wasTie = False }
                                , players =
                                    Array.set
                                        model.activePlayerIndex
                                        { winner | gamesWon = winner.gamesWon + 1 }
                                        model.players
                            }

                        Nothing ->
                            Debug.log "This should never happen..." <|
                                { model | screen = RoundEndScreen { wasTie = True } }

                Board.BoardIsFull ->
                    { model | screen = RoundEndScreen { wasTie = True } }
            , Cmd.none
            )

        ReturnToStartScreen ->
            ( { model
                | screen = StartScreen
                , players = Array.map (\p -> { p | gamesWon = 0 }) model.players
              }
            , Cmd.none
            )

        TickEverySecond ->
            let
                maybeActivePlayer =
                    getActivePlayer model
            in
            ( case maybeActivePlayer of
                Nothing ->
                    model

                Just activePlayer ->
                    { model
                        | players =
                            Array.set
                                model.activePlayerIndex
                                { activePlayer | moveTimerSeconds = activePlayer.moveTimerSeconds + 1 }
                                model.players
                    }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ case model.screen of
            StartScreen ->
                viewStartScreen model.players

            GameScreen ->
                viewGameScreen model

            RoundEndScreen wasTie ->
                viewRoundEndScreen wasTie model
        ]


viewGameScreen : Model -> Html Msg
viewGameScreen model =
    let
        playerText player =
            String.join " / "
                [ player.name
                , "Time Spent: " ++ String.fromInt player.moveTimerSeconds ++ " seconds"
                ]

        viewPlayer { isActivePlayer } player =
            Html.div
                [ HA.style "color" "white"
                , HA.style "text-shadow" "black 0px 0px 10px"
                , HA.style "background-color" <| Color.toHexString player.color
                , HA.style "padding-top" "20px"
                , HA.style "height" "40px"
                , if isActivePlayer then
                    HA.style "border" "black solid 2px"

                  else
                    HA.style "filter" "brightness(50%)"
                ]
                [ Html.text <|
                    if isActivePlayer then
                        String.concat [ "[   ", playerText player, "   ]" ]

                    else
                        playerText player
                ]
    in
    Html.div [] <|
        case getActivePlayer model of
            Nothing ->
                [ Html.text "Error: invalid active player" ]

            Just activePlayer ->
                Board.view (PlayerMadeAMove { activePlayer = activePlayer }) model.board
                    :: (Array.toList <|
                            Array.indexedMap (\i p -> viewPlayer { isActivePlayer = i == model.activePlayerIndex } p) model.players
                       )


viewStartScreen : Array Player -> Html Msg
viewStartScreen players =
    let
        viewPlayerEdit : Int -> Player -> Html Msg
        viewPlayerEdit index player =
            let
                maybePlayerWithSelectedColor color =
                    players |> Array.filter (\p -> p.color == color) |> Array.get 0

                viewColorButton color =
                    Html.button
                        (List.concat
                            [ case maybePlayerWithSelectedColor color of
                                Just p ->
                                    -- Without this you get an annoying effect of whenever
                                    -- a new color is selected the "not-allowed" is displayed
                                    -- straight after
                                    if p == player then
                                        []

                                    else
                                        [ HA.style "cursor" "not-allowed" ]

                                Nothing ->
                                    [ HE.onClick (PlayerEdited { playerIndex = index } { player | color = color })
                                    , HA.style "cursor" "pointer"
                                    ]
                            , [ HA.style "background-color" (Color.toHexString color)
                              , HA.style "height" "20px"
                              , HA.style "width" "20px"
                              , HA.style "margin-left" "5px"
                              ]
                            , if color == player.color then
                                [ HA.style "border" "black inset 3px" ]

                              else
                                [ HA.style "border" "white solid 3px" ]
                            ]
                        )
                        []
            in
            Html.div [] <|
                List.concat
                    [ [ Html.input
                            [ HA.value player.name
                            , HE.onInput (\s -> PlayerEdited { playerIndex = index } { player | name = s })
                            , HA.style "margin-right" "20px"
                            ]
                            []
                      ]
                    , List.map viewColorButton Color.playerColors
                    ]
    in
    Html.div cssGridBox <|
        List.concat
            [ [ Html.h1 [] [ Html.text "Players" ] ]
            , Array.toList <| Array.indexedMap viewPlayerEdit players
            , [ Html.button [ HE.onClick RestartGame ] [ Html.text "Start Game" ] ]
            ]


viewRoundEndScreen : { wasTie : Bool } -> Model -> Html Msg
viewRoundEndScreen { wasTie } model =
    let
        maybeWinner =
            getActivePlayer model

        viewScores player =
            Html.div []
                [ Html.h3 [ HA.style "border-left" <| "solid 10px " ++ Color.toHexString player.color ]
                    [ Html.text player.name ]
                , Html.text <| String.fromInt player.gamesWon
                ]

        cssGridColumns =
            List.map (\( k, v ) -> HA.style k v)
                [ ( "display", "grid" )
                , ( "grid-gap", "50px" )
                , ( "grid-template-columns"
                  , String.join ""
                        [ "repeat("
                        , String.fromInt (Array.length model.players)
                        , ", 100px)"
                        ]
                  )
                , ( "align-content", "center" )
                , ( "justify-content", "center" )
                ]
    in
    Html.div
        cssGridBox
        [ Html.h1 []
            [ Html.text <|
                if wasTie then
                    "Draw: No winner"

                else
                    (maybeWinner |> Maybe.map .name |> Maybe.withDefault "") ++ " won"
            ]
        , Html.div cssGridColumns <|
            Array.toList <|
                Array.map viewScores model.players
        , Html.button [ HE.onClick RestartGame ] [ Html.text "New Game" ]
        , Html.button [ HE.onClick ReturnToStartScreen ] [ Html.text "Back To Start" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        GameScreen ->
            Time.every 1000 (always TickEverySecond)

        _ ->
            Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
