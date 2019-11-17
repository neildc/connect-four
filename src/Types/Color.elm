module Types.Color exposing (Color(..), all, toHexString)


type Color
    = Blue
    | Red
    | Green
    | Yellow


all : List Color
all =
    [ Blue, Red, Green, Yellow ]


toHexString : Color -> String
toHexString color =
    case color of
        Blue ->
            "#0000ff"

        Red ->
            "#ff0000"

        Green ->
            "#00ff00"

        Yellow ->
            "yellow"
