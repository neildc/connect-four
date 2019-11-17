module Main exposing (Model, Msg, init, main, update, view)

import Array exposing (Array)
import Board exposing (Board)
import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Types.Color as Color exposing (Color)
import Types.Player as Player exposing (Player)


const_GAME_COLUMNS : Int
const_GAME_COLUMNS =
    7


const_GAME_ROWS : Int
const_GAME_ROWS =
    6



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
    let
        defaultPlayers =
            List.map (\( name, color ) -> Player.init name color)
                [ ( "Player 1", Color.Blue )
                , ( "Player 2", Color.Red )
                ]
    in
    ( { screen = StartScreen
      , board = Board.init { columns = const_GAME_COLUMNS, rows = const_GAME_ROWS }
      , players = Array.fromList defaultPlayers
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
        viewActivePlayer activePlayer =
            Html.div
                [ HA.style "color" "white"
                , HA.style "background-color" <| Color.toHexString activePlayer.color
                ]
                [ Html.text <| "Active Player: " ++ activePlayer.name
                ]
    in
    Html.div [] <|
        case getActivePlayer model of
            Nothing ->
                [ Html.text "Error: invalid active player" ]

            Just activePlayer ->
                [ Board.view (PlayerMadeAMove { activePlayer = activePlayer }) model.board
                , viewActivePlayer activePlayer
                ]


viewStartScreen : Array Player -> Html Msg
viewStartScreen players =
    let
        viewPlayerEdit : Int -> Player -> Html Msg
        viewPlayerEdit index player =
            let
                viewColorButton color =
                    Html.button
                        (List.concat
                            [ [ HE.onClick (PlayerEdited { playerIndex = index } { player | color = color })
                              , HA.style "background-color" (Color.toHexString color)
                              , HA.style "height" "15px"
                              ]
                            , if color == player.color then
                                [ HA.style "border" "black solid 2px" ]

                              else
                                []
                            ]
                        )
                        []
            in
            Html.div [] <|
                List.concat
                    [ [ Html.input [ HA.value player.name, HE.onInput (\s -> PlayerEdited { playerIndex = index } { player | name = s }) ] []
                      ]
                    , List.map viewColorButton Color.all
                    ]
    in
    Html.div [] <|
        List.concat
            [ [ Html.text "Players" ]
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
                [ Html.text player.name
                , Html.br [] []
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
    Html.div [ HA.style "display" "grid", HA.style "grid-gap" "30px" ] <|
        [ Html.text <|
            if wasTie then
                "Draw: No winner"

            else
                (maybeWinner |> Maybe.map .name |> Maybe.withDefault "") ++ " won"
        , Html.div cssGridColumns <|
            Array.toList <|
                Array.map viewScores model.players
        , Html.button [ HE.onClick RestartGame ] [ Html.text "New Game" ]
        , Html.button [ HE.onClick ReturnToStartScreen ] [ Html.text "Back To Start" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
