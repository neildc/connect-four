module Types.Player exposing (Player, init)

import Types.Color exposing (Color)


init : String -> Color -> Player
init name color =
    { name = name
    , color = color
    , moveTimerSeconds = 0
    , gamesWon = 0
    }


type alias Player =
    { name : String
    , color : Color
    , moveTimerSeconds : Int
    , gamesWon : Int
    }
