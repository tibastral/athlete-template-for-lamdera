module Form exposing
    ( CommonParams
    , FullInputType
    , InputCheckboxContent
    , InputNumberContent
    , InputTextContent
    , InputType(..)
    , buildCheckbox
    , buildDate
    , buildInput
    , buildInputFile
    , buildInputNumber
    , buildInputPassword
    , buildInputText
    , buildInputEmail
    , buildSelect
    , buildTextArea
    , checkBoxSurround
    , errorMessage
    , generateOptions
    , inputField
    , inputLabel
    , inputLabelPlaceholder
    , inputLabelPlaceholderWithError
    , inputLabelWithError
    , inputSurround
    , inputTextStyle
    , labelizedInput
    , DateBetween(..), DateBetweenPosix(..), DateMsg(..), Option, buildNativeDateTime, dateMsgtoPosix, dateTimePickerToPosix, stringToMaybe, timePosixToDatePickerDate, valueForNothing
    )

{-|

@docs CommonParams
@docs FullInputType
@docs InputCheckboxContent
@docs InputNumberContent
@docs InputTextContent
@docs InputType
@docs buildCheckbox
@docs buildDate
@docs buildInput
@docs buildInputFile
@docs buildInputNumber
@docs buildInputPassword
@docs buildInputText
@docs buildInputEmail
@docs buildSelect
@docs buildTextArea
@docs checkBoxSurround
@docs errorMessage
@docs generateOptions
@docs inputField
@docs inputLabel
@docs inputLabelPlaceholder
@docs inputLabelPlaceholderWithError
@docs inputLabelWithError
@docs inputSurround
@docs inputTextStyle
@docs labelizedInput

-}

import BodyBuilder as B exposing (NodeWithStyle)
import BodyBuilder.Attributes as A
import BodyBuilder.Elements.Clickable
import BodyBuilder.Events as E
import BodyBuilder.Extra exposing (fi)
import BodyBuilder.Style as Style
import Color
import DateTime
import DateTimePicker
import DateTimePicker.Config
import Elegant exposing (percent, px)
import Elegant.Block as Block
import Elegant.Border as Border
import Elegant.Box as Box
import Elegant.Constants as Constants
import Elegant.Cursor as Cursor
import Elegant.Display as Display
import Elegant.Extra exposing (..)
import Elegant.Typography as Typography
import Html
import Html.Styled
import Iso8601
import Json.Decode as Decode
import Modifiers exposing (..)
import Time exposing (Month(..))
import Time.Extra


borderColor : Color.Color
borderColor =
    Color.grayscale 0.1


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


{-| Convert an `Int` to a `Month`, with January being 1.
-}
intToMonth : Int -> Maybe Month
intToMonth i =
    case i of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing


timePosixToDatePickerDate : Time.Zone -> Time.Posix -> DateTimePicker.DateTime
timePosixToDatePickerDate zone time =
    let
        ( year, month, day ) =
            Time.Extra.toDateTuple zone time

        ( hours, minutes, _ ) =
            Time.Extra.toTimeTuple zone time
    in
    DateTimePicker.dateTime year month day hours minutes


dateTimePickerToPosix : Time.Zone -> DateTimePicker.DateTime -> Time.Posix
dateTimePickerToPosix zone val =
    Time.Extra.fromDateTuple zone ( val.year, val.month, val.day )
        |> Time.Extra.setHour zone val.hour
        |> Time.Extra.setMinute zone val.minute


buildNativeDateTime : CommonParams -> Time.Zone -> DateTimePicker.State -> Time.Posix -> (( DateTimePicker.State, Maybe DateTimePicker.DateTime ) -> msg) -> NodeWithStyle msg
buildNativeDateTime { label } zone state date msg =
    let
        formattedMessage =
            \a b -> msg ( a, b )

        config =
            DateTimePicker.Config.defaultDateTimePickerConfig formattedMessage

        ht : Html.Html msg
        ht =
            DateTimePicker.dateTimePickerWithConfig
                { config
                    | timePickerType = DateTimePicker.Config.Digital
                    , firstDayOfWeek = Time.Mon
                }
                []
                state
                (Just (date |> timePosixToDatePickerDate zone))
                |> Html.Styled.toUnstyled
    in
    inputSurround Nothing label <|
        B.div []
            [ ( ht, [] ) ]



-- Html.input
--         [ Html.Attributes.value (date |> dateTimeToNativeDateTimeString)
--         , Html.Events.onInput (msg << nativeDateTimeStringToDateTime default)
--         , Html.Attributes.type_ "datetime-local"
--         , Html.Attributes.placeholder label
--         , Html.Attributes.step "60"
--         ]
--         []
--   , []
--   )
-- ]
-- dateTimeToNativeDateTimeString : Time.Posix -> String
-- dateTimeToNativeDateTimeString date =
--     Iso8601.fromTime date
--         |> String.split "."
--         |> List.head
--         |> Maybe.withDefault ""
-- nativeDateTimeStringToDateTime : Time.Posix -> String -> Time.Posix
-- nativeDateTimeStringToDateTime default str =
--     (str ++ ".000Z") |> (Iso8601.toTime >> Result.withDefault default)
-- labelizedInput B.inputEmail commonParams date msg


{-| -}
buildDate : CommonParams -> Maybe DateTime.DateTime -> DateBetween -> (DateMsg -> msg) -> NodeWithStyle msg
buildDate { label } maybeDate between msg =
    let
        years =
            case between of
                DateBetween after before ->
                    List.range
                        (after |> DateTime.getYear)
                        (before |> DateTime.getYear)

        days =
            List.range 1 31

        months =
            List.range 1 12

        -- years =
        --     List.range 1900 2030
        defaultYear =
            2015

        defaultMonth =
            2

        defaultDay =
            1
    in
    case maybeDate of
        Nothing ->
            inputSurround Nothing label <|
                BodyBuilder.Elements.Clickable.monochromeSquaredButton
                    { background = Color.white
                    , border = Color.black
                    , text = Color.black
                    }
                    "Pick a date"
                    (msg SetDefaultDate)

        Just date ->
            inputSurround Nothing label <|
                B.flex [ displayBlock, spaceBetween ]
                    ([ inputField
                        (inputLabel "Year")
                        (Select (generateOptions ((date |> DateTime.getYear) |> String.fromInt) (years |> List.map String.fromInt)) (msg << Year << (Maybe.withDefault defaultYear << String.toInt)) identity)
                     , inputField
                        (inputLabel "Month")
                        (Select (generateOptions ((date |> DateTime.getMonth) |> monthToInt |> String.fromInt) (months |> List.map String.fromInt)) (msg << Month << (Maybe.withDefault defaultMonth << String.toInt)) identity)
                     , inputField
                        (inputLabel "Day")
                        (Select (generateOptions ((date |> DateTime.getDay) |> String.fromInt) (days |> List.map String.fromInt)) (msg << Day << (Maybe.withDefault defaultDay << String.toInt)) identity)
                     ]
                        |> List.map buildInput
                        |> List.map (\e -> fi [ grow ] [ e ])
                        |> List.intersperse (fi [ blockWithWidth Constants.medium ] [])
                    )


{-| -}
buildInput : FullInputType msg a -> NodeWithStyle msg
buildInput object =
    case object.value of
        Text val msg ->
            buildInputText object.commonParams val msg

        Email val msg ->
            buildInputEmail object.commonParams val msg

        TextArea val msg ->
            buildTextArea object.commonParams val msg

        Date val between msg ->
            buildDate object.commonParams val between msg

        NativeDateTime zone state val msg ->
            buildNativeDateTime object.commonParams zone state val msg

        Password val msg ->
            buildInputPassword object.commonParams val msg

        Select list msg stringToA ->
            buildSelect object.commonParams list msg stringToA

        MaybeSelect list msg stringToMaybeA ->
            buildMaybeSelect object.commonParams list msg stringToMaybeA

        Int val msg ->
            buildInputNumber object.commonParams val msg

        Bool val msg ->
            buildCheckbox object.commonParams val msg

        File id msg ->
            buildInputFile object.commonParams id msg


{-| -}
type alias FullInputType msg a =
    { value : InputType msg a
    , commonParams : CommonParams
    }


{-| -}
type alias CommonParams =
    { label : String
    , placeholder : Maybe String
    , error : Maybe String
    }


{-| -}
type DateMsg
    = Day Int
    | Month Int
    | Year Int
    | SetDefaultDate
    | RemoveDate


dateMsgtoPosix : Time.Posix -> DateMsg -> Time.Posix
dateMsgtoPosix date dateMsg =
    let
        actionableDate =
            date |> DateTime.fromPosix
    in
    (case dateMsg of
        Day d ->
            actionableDate |> DateTime.setDay d

        Month m ->
            actionableDate |> DateTime.setMonth (m |> intToMonth |> Maybe.withDefault Jan)

        Year y ->
            actionableDate |> DateTime.setYear y

        _ ->
            Just actionableDate
    )
        |> Maybe.withDefault actionableDate
        |> DateTime.toPosix


type DateBetween
    = DateBetween DateTime.DateTime DateTime.DateTime


type DateBetweenPosix
    = DateBetweenPosix Time.Posix Time.Posix


{-| -}
type InputType msg a
    = Text String (String -> msg)
    | Email String (String -> msg)
    | TextArea String (String -> msg)
    | Password String (String -> msg)
    | Date (Maybe DateTime.DateTime) DateBetween (DateMsg -> msg)
    | NativeDateTime Time.Zone DateTimePicker.State Time.Posix (( DateTimePicker.State, Maybe DateTimePicker.DateTime ) -> msg)
    | Select (List Option) (a -> msg) (String -> a)
    | MaybeSelect (List Option) (Maybe a -> msg) (String -> Maybe a)
    | Int Int (Int -> msg)
    | Bool Bool (Bool -> msg)
    | File String msg


{-| -}
inputField : CommonParams -> InputType msg a -> FullInputType msg a
inputField commonParams value =
    { commonParams = commonParams, value = value }


{-| -}
type alias InputNumberContent msg =
    { label : String
    , value : Int
    , msg : Int -> msg
    }


{-| -}
type alias InputCheckboxContent msg =
    { label : String
    , msg : Bool -> msg
    , checked : Bool
    }


{-| -}
type alias InputTextContent msg =
    { label : String
    , placeholder : String
    , value : String
    , msg : String -> msg
    }


{-| -}
inputLabel : String -> CommonParams
inputLabel labelVal =
    { label = labelVal
    , placeholder = Nothing
    , error = Nothing
    }


{-| -}
inputLabelPlaceholder : String -> String -> CommonParams
inputLabelPlaceholder labelVal placeholder =
    { label = labelVal
    , placeholder = Just placeholder
    , error = Nothing
    }


{-| -}
inputLabelPlaceholderWithError : String -> String -> Maybe String -> CommonParams
inputLabelPlaceholderWithError labelVal placeholder error =
    { label = labelVal
    , placeholder = Just placeholder
    , error = error
    }


{-| -}
inputLabelWithError : String -> Maybe String -> CommonParams
inputLabelWithError labelVal error =
    { label = labelVal
    , placeholder = Nothing
    , error = error
    }


type alias Option =
    { id : String
    , value : String
    , active : Bool
    }


{-| -}
generateOptions : String -> List String -> List Option
generateOptions activeText =
    List.map (\e -> { id = e, value = e, active = e == activeText })


{-| -}
errorMessage : String -> NodeWithStyle msg
errorMessage err =
    B.div
        [ A.style
            [ Style.box
                [ Box.textColor (Color.rgb 126 14 1)
                , typoSize 14
                ]
            ]
        ]
        [ B.text err ]


inputSurroundWithoutPlaceholder error content =
    B.node
        [ A.style
            [ Style.block []
            , Style.box []
            ]
        ]
        [ B.div
            [ A.style
                [ Style.box
                    [ typoSize 14
                    , Box.paddingHorizontal (px 2)
                    ]
                ]
            ]
            [ B.text "" ]
        , content
        , maybeError error
        ]


{-| -}
inputSurround : Maybe String -> String -> NodeWithStyle msg -> NodeWithStyle msg
inputSurround error label content =
    B.node
        [ A.style
            [ Style.block []
            , Style.box []
            ]
        ]
        [ B.div
            [ A.style
                [ Style.box
                    [ typoSize 12
                    , Box.paddingTop Constants.medium
                    , Box.paddingHorizontal Constants.medium
                    , Box.textColor (Color.rgb 145 145 145)
                    ]
                ]
            ]
            [ B.text label ]
        , content
        , maybeError error
        ]


{-| -}
checkBoxSurround : String -> Bool -> (Bool -> msg) -> NodeWithStyle msg
checkBoxSurround label value msg =
    B.node
        [ A.style
            [ Style.block []
            , Style.box [ Box.paddingAll (px 12) ]
            ]
        ]
        [ B.inputCheckbox
            [ E.onCheck msg
            , A.checked value
            ]
        , B.node
            [ E.onClick (msg (not value))
            , A.style
                [ Style.box
                    [ typoSize 14
                    , Box.paddingVertical (px 6)
                    , Box.paddingHorizontal (px 2)
                    , Box.cursor Cursor.pointer
                    ]
                ]
            ]
            [ B.text label ]
        ]


{-| -}
buildInputNumber : CommonParams -> Int -> (Int -> msg) -> NodeWithStyle msg
buildInputNumber { label, error } value msg =
    inputSurround error label <|
        B.inputNumber
            [ A.style
                [ Style.box
                    [ Box.borderColor borderColor
                    , Box.borderSolid
                    , Box.borderWidth 1
                    , Box.cornerRadius 5
                    , Box.paddingAll (px 3)
                    , typoSize 17
                    ]
                ]
            , A.value value
            , E.onInput msg
            ]


{-| -}
buildCheckbox : CommonParams -> Bool -> (Bool -> msg) -> NodeWithStyle msg
buildCheckbox { label } =
    checkBoxSurround label


maybeError error =
    error
        |> Maybe.map errorMessage
        |> Maybe.withDefault B.none


{-| -}
labelizedInput :
    (Modifiers
        (A.BoxContainer
            { a
                | block :
                    Maybe (List ( Modifiers Display.BlockDetails, A.StyleSelector ))
                , fromStringInput : String -> String
                , onInputEvent : Maybe (String -> msg)
                , placeholder : Maybe String
                , value : Maybe String
            }
        )
     -> NodeWithStyle msg
    )
    -> CommonParams
    -> String
    -> (String -> msg)
    -> NodeWithStyle msg
labelizedInput inputFunction { label, placeholder, error } value msg =
    case placeholder of
        Just properPlaceholder ->
            inputSurround error label <|
                B.div []
                    [ inputFunction
                        [ inputTextStyle
                        , A.value value
                        , E.onInput msg
                        , A.placeholder properPlaceholder
                        ]
                    ]

        Nothing ->
            if value == "" then
                inputSurroundWithoutPlaceholder error <|
                    B.div []
                        [ inputFunction
                            [ inputTextStyle
                            , A.value value
                            , E.onInput msg
                            , A.placeholder label
                            ]
                        ]

            else
                inputSurround error label <|
                    B.div []
                        [ inputFunction
                            [ inputTextStyle
                            , A.value value
                            , E.onInput msg
                            , A.placeholder label
                            ]
                        ]


{-| -}
buildInputText : CommonParams -> String -> (String -> msg) -> NodeWithStyle msg
buildInputText =
    labelizedInput B.inputText


{-| -}
buildInputEmail : CommonParams -> String -> (String -> msg) -> NodeWithStyle msg
buildInputEmail =
    labelizedInput B.inputEmail


{-| -}
buildInputPassword : CommonParams -> String -> (String -> msg) -> NodeWithStyle msg
buildInputPassword =
    labelizedInput B.inputPassword


{-| -}
buildTextArea : CommonParams -> String -> (String -> msg) -> NodeWithStyle msg
buildTextArea =
    labelizedInput B.textarea


{-| -}
buildInputFile : CommonParams -> String -> msg -> NodeWithStyle msg
buildInputFile { label, error } inputFileId msg =
    inputSurround error label <|
        B.inputFile
            [ A.id inputFileId
            , E.on "change" (Decode.succeed msg)
            ]


{-| -}
buildSelect : CommonParams -> List { b | active : Bool, id : String, value : String } -> (a -> msg) -> (String -> a) -> NodeWithStyle msg
buildSelect { label, error } options msg stringToA =
    inputSurround error label <|
        B.select
            [ A.style
                [ Style.box
                    [ Box.typography
                        [ Typography.size (px 14) ]
                    , Box.borderNone
                    , Box.backgroundColor (Color.rgba 0 0 0 0)
                    ]
                , Style.block [ Block.fullWidth ]
                ]
            , E.onInput (msg << stringToA)
            ]
            (options |> List.map (\e -> B.option e.id e.value e.active))


valueForNothing : String
valueForNothing =
    "---Nothing---"


stringToMaybe : (String -> a) -> String -> Maybe a
stringToMaybe fun str =
    if str == valueForNothing then
        Nothing

    else
        Just (fun str)


buildMaybeSelect : CommonParams -> List { b | active : Bool, id : String, value : String } -> (Maybe a -> msg) -> (String -> Maybe a) -> NodeWithStyle msg
buildMaybeSelect { label, error } options msg stringToMaybeA =
    inputSurround error label <|
        B.select
            [ A.style
                [ Style.box
                    [ Box.typography
                        [ Typography.size (px 14) ]
                    , Box.borderNone
                    , Box.backgroundColor (Color.rgba 0 0 0 0)
                    ]
                , Style.block [ Block.fullWidth ]
                ]
            , E.onInput (msg << stringToMaybeA)
            ]
            (options |> List.map (\e -> B.option e.id e.value e.active))


{-| -}
inputTextStyle : Modifiers.Modifier (A.BoxContainer { a | block : Maybe (List ( Modifiers.Modifiers Display.BlockDetails, A.StyleSelector )) })
inputTextStyle =
    A.style
        [ Style.box
            [ Box.paddingAll Constants.medium
            , typoSize 17
            , Box.borderNone
            , Box.backgroundColor (Color.rgba 0 0 0 0)
            ]
        , Style.block [ Block.width (percent 100) ]
        , [ Box.typography
                [ Typography.color Color.white
                , Typography.size (px 48)
                ]
          ]
            |> Style.box
            |> Style.pseudoClass "placeholder"
        ]
