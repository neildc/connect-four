module Game exposing (Game, Msg, init, view)

import Array exposing (Array)
import Html exposing (Html)
import Types.Player exposing (Player)


const_GAME_COLUMNS : Int
const_GAME_COLUMNS =
    7


const_GAME_ROWS : Int
const_GAME_ROWS =
    6



init : Array Player -> Game
init players =
    let
        grid =
            Array.repeat const_GAME_COLUMNS
                (Array.repeat const_GAME_ROWS Nothing)
    in
    { grid = grid
    , players = players
    , activePlayerIndex = 0
    }




type alias Game =


type Msg
    = PlayerMadeAMove PlaceResult
    | GameFinished { winner : Maybe Player }
    | RestartGame (Array Player)


update : Msg -> Game -> Maybe Game
update msg model =
    Just model


view : Game -> Html Msg
view model =
    Html.text ""
