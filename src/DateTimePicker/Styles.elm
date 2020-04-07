module DateTimePicker.Styles exposing (..)

{-| DateTimePicker.Css

Using [rtfeldman/elm-css](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest)
Include this in your elm-css port module to be included in your project's css file.


# Css

@docs css

-}

import Css exposing (..)


timePickerDialog : Style
timePickerDialog =
    Css.batch
        [ float left

        -- , height calendarHeight
        , textAlign center
        , borderLeft3 (px 1) solid darkGray
        ]


timeHeaderStyle : Style
timeHeaderStyle =
    Css.batch
        [ padding2 (px 3) (px 10)
        , marginTop (px 3)
        , marginBottom (px 3)
        , display inlineBlock
        , cursor pointer
        ]


amPmStyle : Style
amPmStyle =
    Css.batch
        [ fontSize (Css.em 1.2)
        , padding2 (Css.em 1) (Css.em 0)
        , cursor pointer
        , margin2 (px 0) auto
        , width (px 85)
        , hover [ backgroundColor highlightedDay ]
        ]


highlightStyle : Css.Style
highlightStyle =
    Css.batch
        [ property "box-shadow" "inset 0 0 10px 3px #3276b1"
        , backgroundColor selectedDate
        , color (hex "#fff")
        , highlightBorderStyle
        ]


highlightSelectedDay : Css.Color
highlightSelectedDay =
    hex "#d5e5f3"


selectedDate : Css.Color
selectedDate =
    hex "#428bca"


fadeText : Css.Color
fadeText =
    hex "#a1a1a1"


lightGray : Css.Color
lightGray =
    hex "#f5f5f5"


darkGray : Css.Color
darkGray =
    hex "#ccc"


highlightedDay : Css.Color
highlightedDay =
    hex "#ebebeb"


dayStyle : Css.Style
dayStyle =
    Css.batch
        [ cellStyle
        , textAlign right
        ]


cellStyle : Css.Style
cellStyle =
    Css.batch
        [ padding4 (px 7) (px 7) (px 7) (px 9)
        , border (px 0)
        , cursor pointer
        ]


arrowStyle : Css.Style
arrowStyle =
    Css.batch
        [ borderBoxStyle
        , textAlign center
        , transform (scale 0.8)
        , position absolute
        , padding2 (px 0) (px 8)
        , cursor pointer
        ]


borderBoxStyle : Css.Style
borderBoxStyle =
    Css.batch [ boxSizing borderBox ]


highlightBorderStyle : Css.Style
highlightBorderStyle =
    Css.batch [ borderRadius (px 0) ]


headerStyle : Css.Style
headerStyle =
    Css.batch
        [ padding2 (px 10) (px 7)
        , backgroundColor lightGray
        ]


calendarHeight : Css.Px
calendarHeight =
    px 277


tableStyle : Css.Style
tableStyle =
    Css.batch
        [ property "border-spacing" "0"
        , property "border-width" "0"
        , property "table-layout" "fixed"
        , margin (px 0)
        ]


activeStyle : Css.Style
activeStyle =
    Css.batch
        [ backgroundColor (hex "#e0e0e0")
        , highlightBorderStyle
        ]


emptyCellStyle : Css.Style
emptyCellStyle =
    Css.batch [ hover [ backgroundColor unset ], cursor unset ]
