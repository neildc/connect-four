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
    { id : Int
    , name : String
    , color : Color
    , moveTimerSeconds : Int
    }


type alias Board =
    { grid : Array (Array (Maybe { playerId : Int }))
    }


type Screen
    = StartScreen
    | GameScreen { board : Board, activePlayerTurn : Player }
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
            [ { id = 0
              , name = "Player 1"
              , color = Blue
              , moveTimerSeconds = 0
              }
            , { id = 1
              , name = "Player 2"
              , color = Red
              , moveTimerSeconds = 0
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
    = NoOp


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
