module Main exposing (Board, Color(..), Model, Msg(..), Player, Screen(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----


type Color
    = Blue
    | Red


type alias Player =
    { name : String
    , color : Color
    , moveTimerSeconds : Int
    , gamesWon : Int
    }


type alias Board =
    { grid : Array (Array (Maybe { playerIndex : Int }))
    }


type PlaceResult
    = -- TODO: think of a better name
      ValidDrop Board
    | ColumnAlreadyFull
    | WinningMove Board
    | BoardIsFull



-- place : { column : Int } -> Board -> PlaceResult
-- isFilled : { column : Int } -> Board -> Bool
-- isWinningMove : { column : Int } -> Board -> Bool


type Screen
    = StartScreen
    | GameScreen { board : Board, activePlayerIndex : Int }
    | RoundEndScreen
        { winner : Maybe Player -- might be a tie
        , scorePlayers : List Int -- should we handle more than a 2 players?
        }


type alias Model =
    { board : Board
    , players : List Player
    , screen : Screen
    }


init : ( Model, Cmd Msg )
init =
    let
        players =
            [ { name = "Player 1"
              , color = Blue
              , moveTimerSeconds = 0
              , gamesWon = 0
              }
            , { name = "Player 2"
              , color = Red
              , moveTimerSeconds = 0
              , gamesWon = 0
              }
            ]
    in
    ( { players = players
      , board = { grid = Array.empty }
      , screen = StartScreen
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = PlayerNamesAndColorsConfigured -- StartScreen
    | NewTurn -- GameScreen
    | PlayerMadeAMove -- GameScreen
    | GameFinished -- GameScreen
    | RestartGame -- RoundEndScreen
    | ReturnToStartScreen --RoundEndScreen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
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
