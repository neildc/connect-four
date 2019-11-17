module Types.Color exposing (Color(..), playerColors, toHexString)


type Color
    = Blue
    | Red
    | Green
    | Yellow
    | Orange
    | LightGray
    | Black


playerColors : List Color
playerColors =
    [ Blue, Red, Green, Yellow, Orange ]


toHexString : Color -> String
toHexString color =
    -- https://coolors.co/b3001b-262626-255c99-fcfcfc-d2f898
    case color of
        Blue ->
            "#255C99"

        Red ->
            "#B3001B"

        Green ->
            "#D2F898"

        Yellow ->
            "#F7F052"

        Orange ->
            "#F28123"

        LightGray ->
            "#FCFCFC"

        Black ->
            "#262626"
