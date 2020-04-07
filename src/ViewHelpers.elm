module ViewHelpers exposing (..)

-- import Remote.Object.MeetingTemplate

import BodyBuilder as B exposing (NodeWithStyle)
import BodyBuilder.Attributes as A
import BodyBuilder.Events as E
import BodyBuilder.Extra as Layout
import BodyBuilder.Style as Style
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Color
import DateFormat
import Dict
import Elegant exposing (percent, px)
import Elegant.Background as Background
import Elegant.Block as Block
import Elegant.Border as Border
import Elegant.Box as Box
import Elegant.Constants as Constants
import Elegant.Display as Display
import Elegant.Extra
    exposing
        ( alignCenter
        , block
        , blockProperties
        , blockWithWidth
        , bold
        , border
        , box
        , cursorPointer
        , displayBlock
        , fontSize
        , grow
        , padding
        , paddingAll
        , paddingBottom
        , paddingHorizontal
        , paddingTop
        , paddingVertical
        , typoSize
        , typography
        )
import Elegant.Grid as Grid
import Elegant.Margin as Margin
import Elegant.Overflow as Overflow
import Elegant.Padding as Padding
import Elegant.Position as Position
import Elegant.Shadow as Shadow
import Elegant.Typography as Typography
import Form
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument
import Graphql.SelectionSet
import Html
import Html.Attributes
import Html.Parser
import Html.Parser.Util
import Ionicon.Android
import Ionicon.Ios
import Ionicon.Social
import Json.Decode
import Json.Encode
import List.Extra
import Markdown
import Modifiers exposing (Modifier)
import OrientedLayout exposing (..)
import Process
import Random
import Task
import Time exposing (Weekday(..))
import Time.Extra
import Url
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query
import Uuid


spacer : Int -> NodeWithStyle msg
spacer size =
    B.div [ A.style [ Style.box [ Box.paddingAll (px size) ] ] ] []


boxShadow e =
    box [ Box.boxShadow e ]


linkChip : Color.Color -> Color.Color -> String -> List (Cell msg) -> NodeWithStyle msg
linkChip bgColor fgColor url content =
    centeredRow []
        [ autoCell []
            (B.a
                [ backgroundColor bgColor
                , paddingAll Constants.small
                , cornerRadius 100
                , textColor fgColor
                , cursorPointer
                , block []
                , boxShadow [ Shadow.standard (px 6) (Color.rgba 0 0 0 0.16) ( px 0, px 3 ) ]
                , A.href url
                , noDecoration
                , targetBlank
                ]
                [ centeredRow [] content ]
            )
        , fill
        ]


chip : Color.Color -> Color.Color -> msg -> List (Cell msg) -> NodeWithStyle msg
chip bgColor fgColor msg content =
    centeredRow []
        [ autoCell []
            (centeredRow
                [ backgroundColor bgColor
                , paddingAll Constants.small
                , cornerRadius 100
                , textColor fgColor
                , E.onClick msg
                , cursorPointer
                , boxShadow [ Shadow.standard (px 6) (Color.rgba 0 0 0 0.16) ( px 0, px 3 ) ]
                ]
                content
            )
        , fill
        ]


borderedChip : Color.Color -> Color.Color -> Color.Color -> msg -> List (Cell msg) -> NodeWithStyle msg
borderedChip bgColor fgColor borderColor msg content =
    centeredRow []
        [ autoCell []
            (centeredRow
                [ backgroundColor bgColor
                , paddingAll Constants.small
                , cornerRadius 100
                , textColor fgColor
                , E.onClick msg
                , cursorPointer
                , border [ Border.all (borderStandard 3 borderColor) ]
                , boxShadow [ Shadow.standard (px 6) (Color.rgba 0 0 0 0.16) ( px 0, px 3 ) ]
                ]
                content
            )
        , fill
        ]


viewWithPopupHelper : NodeWithStyle msg -> NodeWithStyle msg -> msg -> NodeWithStyle msg
viewWithPopupHelper content popupContent hideMsg =
    B.grid
        [ A.style
            [ Style.block [ Block.height (Elegant.vh 100) ]
            , Style.gridContainerProperties
                [ Grid.columns
                    [ Grid.template
                        [ Grid.simple (Grid.fractionOfAvailableSpace 1)
                        , Grid.simple (Grid.sizeUnitVal (px 400))
                        , Grid.simple (Grid.fractionOfAvailableSpace 1)
                        ]
                    , Grid.align Grid.stretch
                    , Grid.alignItems (Grid.alignWrapper Grid.stretch)
                    ]
                , Grid.rows
                    [ Grid.template
                        [ Grid.simple (Grid.fractionOfAvailableSpace 1)
                        , Grid.simple (Grid.sizeUnitVal (px 400))
                        , Grid.simple (Grid.fractionOfAvailableSpace 1)
                        ]
                    , Grid.align Grid.stretch
                    , Grid.alignItems (Grid.alignWrapper Grid.stretch)
                    ]
                ]
            , Style.box []
            ]
        , overflowHidden
        ]
        ([ B.gridItem
            [ A.style
                [ Style.gridItemProperties
                    [ Grid.horizontal [ Grid.placement 0, Grid.size (Grid.span 3) ]
                    , Grid.vertical [ Grid.placement 0, Grid.size (Grid.span 3) ]
                    ]
                ]
            ]
            [ content
            ]
         ]
            ++ [ B.gridItem
                    [ A.style
                        [ Style.gridItemProperties
                            [ Grid.horizontal [ Grid.placement 0, Grid.size (Grid.span 3) ]
                            , Grid.vertical [ Grid.placement 0, Grid.size (Grid.span 3) ]
                            ]
                        ]
                    ]
                    [ centeredRow
                        ([ backgroundColor (Color.rgba 1 1 1 0.16)
                         , box
                            [ Box.position
                                (Position.absolute
                                    []
                                )
                            ]
                         ]
                            ++ [ E.onClick hideMsg ]
                        )
                        [ autoCell []
                            B.none
                        ]
                    ]
               , B.gridItem
                    [ A.style
                        [ Style.gridItemProperties
                            [ Grid.horizontal [ Grid.placement 1, Grid.size (Grid.span 1) ]
                            , Grid.vertical [ Grid.placement 1, Grid.size (Grid.span 1) ]
                            ]
                        ]
                    ]
                    [ centeredRow
                        [ box
                            []
                        ]
                        [ fillCell [ A.style [ Style.box [] ] ]
                            (B.div
                                [ backgroundColor Color.white
                                , box
                                    [ Box.boxShadow [ Shadow.standard (px 6) (Color.rgba 0 0 0 0.16) ( px 0, px 3 ) ]
                                    , Box.cornerRadius 6
                                    , Box.position
                                        (Position.relative
                                            []
                                        )
                                    ]
                                ]
                                [ popupContent ]
                            )
                        ]
                    ]
               ]
        )


viewWithPopup : NodeWithStyle msg -> NodeWithStyle msg -> msg -> Bool -> NodeWithStyle msg
viewWithPopup content popupContent hideMsg showPopup =
    if showPopup then
        viewWithPopupHelper content popupContent hideMsg

    else
        content


inputBgColor : Color.Color
inputBgColor =
    Color.rgb 249 249 252


characterRound size c =
    autoCell
        [ paddingTop (px 3)
        , backgroundColor (Color.grayscale 0.4)
        , textColor Color.white
        , cornerRadius 100
        , block [ Block.alignCenter, Block.height (px size), Block.width (px size) ]
        , box [ typoSize (round ((size |> toFloat) / 1.4)) ]
        ]
        (B.text c)


avatarView size user =
    let
        prettyUser =
            userName user
    in
    user.avatarUrl
        |> Maybe.map
            (\url ->
                pxCell size
                    [ A.rawAttribute (Html.Attributes.style "background-size" "cover")
                    , A.rawAttribute (Html.Attributes.style "height" "auto")
                    , A.rawAttribute (Html.Attributes.style "background-image" ("url(" ++ url ++ ")"))
                    , cornerRadius 1000
                    ]
                    (B.img ""
                        url
                        [ block
                            [ Block.width (px size)
                            , Block.height (px size)
                            ]
                        , cornerRadius 1000
                        , box [ Box.opacity 0 ]
                        ]
                    )
            )
        |> Maybe.withDefault
            (characterRound
                size
                (prettyUser
                    |> String.toList
                    |> List.head
                    |> Maybe.withDefault 'A'
                    |> String.fromChar
                    |> String.toUpper
                )
            )


userName user =
    let
        name =
            (case user.firstName of
                Nothing ->
                    []

                Just firstName ->
                    [ firstName ]
            )
                ++ (case user.lastName of
                        Nothing ->
                            []

                        Just lastName ->
                            [ lastName ]
                   )
    in
    if name |> List.isEmpty then
        user.email

    else
        name |> String.join " "


mailtoLink : String -> String -> String
mailtoLink subject content =
    "mailto:?subject="
        ++ subject
        ++ "&body="
        ++ (content |> String.replace "\n" "%0D%0A")


type alias BoxContainerModifier a =
    Modifier (A.BoxContainer a)


overflowHidden : Modifiers.Modifier (A.MaybeBlockContainer a)
overflowHidden =
    block [ Block.overflowHidden ]


paddingRight : Elegant.SizeUnit -> BoxContainerModifier a
paddingRight val =
    padding [ Padding.right val ]


paddingLeft : Elegant.SizeUnit -> BoxContainerModifier a
paddingLeft val =
    padding [ Padding.left val ]


beautifullyStyledMainView : ( B.Node msg, List String ) -> NodeWithStyle msg
beautifullyStyledMainView a =
    row
        [--  typography
         -- [ Typography.fontFamilySansSerif
         -- ]
        ]
        [ fillCell []
            (B.div [ blockProperties [ Block.height (Elegant.vh 100), Block.width (Elegant.vw 100) ] ]
                [ ( Html.node "style"
                        []
                        [ Html.text "*{box-sizing: border-box;} .fe4pJf{border-right: #e0e0e0 1px solid} body {margin: 0px; height: 100%; display: grid} html {height: 100%; display: grid} textarea {resize : vertical} bb-grid-item {display: grid}"

                        -- , Html.text "* {outline: 1px solid red; }"
                        ]
                  , []
                  )
                , a
                ]
            )
        ]


microsoftInputStyleWithGrayPlaceholder : Bool -> Modifiers.Modifier (A.BoxContainer a)
microsoftInputStyleWithGrayPlaceholder =
    microsoftInputStyle (Color.grayscale 0.3)


microsoftInputStyle : Color.Color -> Bool -> Modifiers.Modifier (A.BoxContainer a)
microsoftInputStyle placeholderColor isTextarea =
    let
        borderMethod =
            if isTextarea then
                Border.all

            else
                Border.bottom
    in
    A.style
        [ Style.box
            [ Box.borderNone
            , Box.paddingAll Constants.small
            , Box.typography [ Typography.size (px 14) ]
            , Box.textColor (Color.rgb 51 51 51)
            , Box.border
                [ borderMethod
                    [ Border.color (Color.rgb 149 149 149)
                    , Border.solid
                    , Border.thickness (px 1)
                    ]
                ]
            ]
        , Style.box
            [ Box.outlineNone
            , Box.border
                [ borderMethod
                    [ Border.color (Color.rgb 12 97 205)
                    , Border.solid
                    , Border.thickness (px 1)
                    ]
                ]
            ]
            |> Style.focus
        , Style.box
            [ Box.border
                [ borderMethod
                    [ Border.color Color.black
                    , Border.solid
                    , Border.thickness (px 1)
                    ]
                ]
            ]
            |> Style.hover
        , Style.box
            [ Box.textColor placeholderColor ]
            |> Style.pseudoClass ":placeholder"
        ]


sticky =
    stickyFrom 0


stickyFrom val =
    box
        [ Box.position
            (Position.sticky [ Position.top (px val) ])
        ]


blueButton : String -> msg -> NodeWithStyle msg
blueButton =
    googleButton True Color.blue Color.white


emptyButton =
    googleButton True (Color.rgba 0 0 0 0) Color.black


googleButton : Bool -> Color.Color -> Color.Color -> String -> msg -> NodeWithStyle msg
googleButton isBlock bgColor tColor text msg =
    B.node
        ([ E.onClick msg
         , cursorPointer
         , backgroundColor bgColor
         , textColor tColor
         , cornerRadius 6
         , paddingAll
            Constants.medium

         --  , A.style
         --     [ Style.box
         --         [ Box.textColor Color.black
         --         ]
         --         |> Style.hover
         -- ]
         ]
            ++ (if isBlock then
                    [ block [ Block.alignCenter ] ]

                else
                    []
               )
        )
        [ B.span
            []
            [ B.text text
            ]
        ]


transparentBorder =
    [ Border.thickness (px 1), Border.solid, Border.color (Color.rgba 0 0 0 0) ]


msButton : Bool -> Color.Color -> List (NodeWithStyle msg) -> msg -> NodeWithStyle msg
msButton isBlock color content msg =
    B.button
        ([ E.onClick msg
         , cursorPointer
         , A.style
            [ Style.box
                [ Box.backgroundColor color
                , Box.textColor Color.white
                ]
            ]
         , paddingVertical Constants.small
         , fontSize (px 14)
         , border [ Border.all transparentBorder ]
         ]
            ++ (if isBlock then
                    [ block [ Block.alignCenter ] ]

                else
                    []
               )
        )
        content


googleLink : Bool -> Color.Color -> NodeWithStyle msg -> String -> NodeWithStyle msg
googleLink isBlock color content href =
    B.a
        ([ A.href href
         , cursorPointer
         , typography [ Typography.noDecoration ]
         , block []
         , A.style
            [ Style.box
                [ Box.backgroundColor color
                , Box.textColor Color.white
                , Box.cornerRadius 4
                ]
            ]
         , paddingAll
            Constants.medium
         ]
            ++ (if isBlock then
                    [ block [ Block.alignCenter ] ]

                else
                    []
               )
        )
        [ content
        ]


cssColorString : String -> List String -> String
cssColorString kind values =
    kind ++ "(" ++ String.join ", " values ++ ")"


colorToCssRgba : Color.Color -> String
colorToCssRgba cl =
    let
        { red, green, blue, alpha } =
            Color.toRgb cl
    in
    cssColorString "rgba"
        [ String.fromInt red
        , String.fromInt green
        , String.fromInt blue
        , String.fromFloat alpha
        ]


icon iconSvg color =
    iconSvg color


{-| -}
type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


toRGBA { red, green, blue, alpha } =
    { red = (red |> toFloat) / 255
    , green = (green |> toFloat) / 255
    , blue = (blue |> toFloat) / 255
    , alpha = alpha
    }


{-| -}
ionIcon : (number -> RGBA -> Html.Html msg) -> Color.Color -> NodeWithStyle msg
ionIcon =
    ionIconWithSize 24


ionIconWithSize size fun color =
    ( fun size (Color.toRgb color |> toRGBA)
    , []
    )


leftIconWithDescription iconSvg description color =
    centeredColumn
        []
        [ autoCell []
            (icon iconSvg color)
        , autoCell
            [ typography [ Typography.color color ] ]
            description
        ]


iconWithDescription iconSvg description color =
    rowWithOptions []
        []
        [ autoCell []
            (icon iconSvg color)
        , fillCell
            [ typography [ Typography.color color ] ]
            description
        ]


leftIonIconWithDescription ionIconFun description color =
    columnWithOptions []
        []
        [ autoCell []
            (ionIcon ionIconFun color)
        , fillCell
            [ typography [ Typography.color color ] ]
            description
        ]


ionIconWithDescription ionIconFun description color =
    rowWithOptions []
        []
        [ autoCell []
            (ionIcon ionIconFun color)
        , fillCell
            [ typography [ Typography.color color ] ]
            description
        ]


ionIconWithDescriptionAndSize ( size, myFontSize ) ionIconFun description color =
    rowWithOptions []
        [ fontSize myFontSize ]
        [ autoCell []
            (ionIconWithSize size ionIconFun color)
        , fillCell
            [ typography [ Typography.color color ] ]
            description
        ]


noDecoration =
    typography [ Typography.noDecoration ]


leftMenuItem : Url.Url -> Color.Color -> Color.Color -> String -> (Color.Color -> NodeWithStyle msg) -> Cell msg
leftMenuItem currentUrl colorActive colorPassive path textAndIcon =
    leftMenuItemMulti currentUrl colorActive colorPassive path [] textAndIcon


transparent =
    Color.rgba 0 0 0 0


cornerRadius val =
    box [ Box.cornerRadius val ]


backgroundColor val =
    box [ Box.backgroundColor val ]


textColor val =
    box [ Box.textColor val ]


beautifulShadow =
    boxShadow [ Shadow.standard (px 6) (Color.rgba 0 0 0 0.16) ( px 0, px 3 ) ]


leftMenuItemMulti : Url.Url -> Color.Color -> Color.Color -> String -> List String -> (Color.Color -> NodeWithStyle msg) -> Cell msg
leftMenuItemMulti currentUrl colorActive colorPassive path startsWithElements textAndIcon =
    let
        isCurrentUrl =
            currentUrl.path == path

        isActive =
            isCurrentUrl || (startsWithElements |> List.any (\e -> currentUrl.path |> String.startsWith e))

        transparentBorder5 =
            [ Border.thickness (px 5), Border.solid, Border.color transparent ]

        whiteBorder5 =
            [ Border.thickness (px 5), Border.solid, Border.color Color.white ]
    in
    autoCell
        [ border
            [ Border.right transparentBorder5
            , Border.left
                (if isActive then
                    whiteBorder5

                 else
                    transparentBorder5
                )
            ]
        , backgroundColor
            (Color.rgba 255
                255
                255
                (if isActive then
                    0.16

                 else
                    0
                )
            )
        ]
        (if isCurrentUrl then
            B.span
                [ typography [ Typography.noDecoration, Typography.size (px 13) ]
                , textColor Color.white
                , paddingHorizontal (px 12)
                ]
                [ textAndIcon Color.white ]

         else
            B.a
                [ A.href path
                , typography [ Typography.noDecoration, Typography.size (px 13) ]
                , textColor Color.white
                , paddingHorizontal (px 12)
                ]
                [ textAndIcon Color.white ]
        )


targetBlank =
    A.target "_blank"


mainBorder =
    [ Border.thickness (px 1), Border.solid, Border.color (Color.rgb 158 158 207) ]


electricGray : Color.Color
electricGray =
    Color.rgb 51 48 59


buttonColor =
    electricGray


borderStandard thickness color =
    [ Border.thickness (px thickness), Border.color color, Border.solid ]


panelTitle : String -> NodeWithStyle msg
panelTitle text =
    B.div
        [ fontSize (px 24)
        , paddingTop Constants.large
        , paddingBottom Constants.medium
        ]
        [ B.text text ]


togglablePanel showPanel title content msg =
    B.div []
        [ centeredRow [ E.onClick msg, cursorPointer ]
            [ autoCell
                [ paddingRight Constants.medium
                , paddingTop Constants.large
                , paddingBottom Constants.medium
                , fontSize (px 12)
                ]
                (B.text
                    (if showPanel then
                        "▼"

                     else
                        "▲"
                    )
                )
            , fillCell [] (panelTitle title)
            ]
        , if showPanel then
            B.div [] content

          else
            B.none
        ]


linkTo : String -> String -> NodeWithStyle msg
linkTo label url =
    B.a
        [ A.href url
        , textColor <| electricGray
        , typography [ Typography.noDecoration ]
        ]
        [ B.text label
        ]


linkToMsg : String -> msg -> NodeWithStyle msg
linkToMsg label msg =
    B.span
        [ textColor <| electricGray
        , typography [ Typography.noDecoration ]
        , E.onClick msg
        , cursorPointer
        ]
        [ B.text label ]


coloredLinkToMsg : Color.Color -> Color.Color -> String -> msg -> NodeWithStyle msg
coloredLinkToMsg bgColor color label msg =
    B.div
        [ paddingVertical Constants.small
        , paddingHorizontal Constants.medium
        , cornerRadius 6
        , backgroundColor bgColor
        , textColor <| color
        , typography [ Typography.noDecoration ]
        , E.onClick msg
        , cursorPointer
        ]
        [ B.text label ]


externalColoredLinkTo : Color.Color -> String -> String -> NodeWithStyle msg
externalColoredLinkTo color label url =
    B.a
        [ A.href url
        , textColor <| color
        , A.target "_blank"
        , typography [ Typography.noDecoration ]
        ]
        [ B.text label
        ]


coloredLinkTo : Color.Color -> String -> String -> NodeWithStyle msg
coloredLinkTo color label url =
    B.a
        [ A.href url
        , textColor <| color
        , typography [ Typography.noDecoration ]
        ]
        [ B.text label
        ]


zIndex a =
    box [ Box.zIndex a ]


centeredView content =
    OrientedLayout.columnWithOptions [ OrientedLayout.centeredContent ] [] [ OrientedLayout.fillCell [] (OrientedLayout.centeredRow [] [ OrientedLayout.fillCell [] content ]) ]


ariane : String -> Cell msg
ariane text =
    autoCell [ paddingAll Constants.huge ] (B.text text)


tagView : { a | name : String, color : Color.Color } -> B.NodeWithStyle msg
tagView tag =
    B.div
        [ paddingHorizontal Constants.large
        , paddingVertical Constants.medium
        , cornerRadius 4
        , backgroundColor tag.color
        , textColor
            (tag.color
                |> Color.toHsl
                |> (\{ lightness } ->
                        -- https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
                        if lightness > 0.179 then
                            Color.black

                        else
                            Color.white
                   )
            )
        ]
        [ B.text tag.name ]


buttonShadow =
    beautifulShadow


titleFontSize =
    fontSize (px 24)


semiBold =
    typography [ Typography.weight 600 ]


todayCircleMargin =
    box [ Box.margin [ Margin.left (Margin.width (px -12)) ] ]


buttonWithDeleteAction : String -> msg -> B.NodeWithStyle msg
buttonWithDeleteAction title buttonMsg =
    B.div
        [ paddingHorizontal Constants.large
        , paddingVertical Constants.medium
        , cornerRadius 50
        , backgroundColor Color.white
        , textColor
            Color.black
        , E.onClick buttonMsg
        , cursorPointer
        , buttonShadow
        ]
        [ B.text title ]


tagViewWithDeleteButton : { a | name : String, color : Color.Color } -> B.NodeWithStyle msg
tagViewWithDeleteButton tag =
    B.div
        [ paddingHorizontal Constants.large
        , paddingVertical Constants.medium
        , cornerRadius 4
        , backgroundColor tag.color
        , textColor
            (tag.color
                |> Color.toHsl
                |> (\{ lightness } ->
                        -- https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
                        if lightness > 0.179 then
                            Color.black

                        else
                            Color.white
                   )
            )
        ]
        [ B.text tag.name ]


prm =
    paddingRight Constants.medium


prl =
    paddingRight Constants.large


pbh =
    paddingBottom Constants.huge


pah =
    paddingAll Constants.huge


pam =
    paddingAll Constants.medium


pas =
    paddingAll Constants.small


pbm =
    paddingBottom Constants.medium


ptm =
    paddingTop Constants.medium


fs num =
    fontSize (px num)


htmlIdOf : ( Int, String, Int ) -> String
htmlIdOf ( year, month, day ) =
    "today-" ++ (year |> String.fromInt) ++ month ++ (day |> String.fromInt)


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


scrollToId : msg -> String -> Cmd msg
scrollToId noOp id =
    Dom.getElement id
        |> Task.andThen
            (\info -> Dom.setViewportOf "html" 0 info.element.y)
        |> Task.attempt
            (\_ -> noOp)


fullMonthName : Time.Month -> String
fullMonthName month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


monthToNumber month =
    case month of
        "January" ->
            1

        "February" ->
            2

        "March" ->
            3

        "April" ->
            4

        "May" ->
            5

        "June" ->
            6

        "July" ->
            7

        "August" ->
            8

        "September" ->
            9

        "October" ->
            10

        "November" ->
            11

        "December" ->
            12

        _ ->
            0


displayDayOfWeek dayOfWeek =
    case dayOfWeek of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


beginningOfMonth : Time.Zone -> Time.Posix -> ( Int, String )
beginningOfMonth timeZone startsAt =
    let
        month =
            Time.toMonth timeZone startsAt |> fullMonthName

        year =
            Time.toYear timeZone startsAt
    in
    ( year, month )


beginningOfDay : Time.Zone -> Time.Posix -> ( Int, String, Int )
beginningOfDay timeZone startsAt =
    let
        day =
            Time.toDay timeZone startsAt

        month =
            Time.toMonth timeZone startsAt |> fullMonthName

        year =
            Time.toYear timeZone startsAt
    in
    ( year, month, day )


closerTo : Time.Posix -> List Time.Posix -> Time.Posix
closerTo now times =
    let
        ( a, b ) =
            (times
                |> List.reverse
                |> List.partition (\e -> Time.posixToMillis e < Time.posixToMillis now)
            )
                |> Tuple.mapBoth
                    (\before -> before |> List.sortBy Time.posixToMillis |> List.reverse |> List.head)
                    (\after -> after |> List.sortBy Time.posixToMillis |> List.head)
    in
    a
        |> Maybe.withDefault
            (b
                |> Maybe.withDefault now
            )
