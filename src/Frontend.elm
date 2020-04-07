module Frontend exposing (..)

import BodyBuilder as B exposing (NodeWithStyle)
import BodyBuilder.Attributes as A
import BodyBuilder.Events as E
import BodyBuilder.Extra as Layout
import BodyBuilder.Style as Style
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Color
import Elegant exposing (px)
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
        , marginAuto
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
import Elegant.Padding as Padding
import Elegant.Position as Position
import Elegant.Shadow as Shadow
import Elegant.Typography as Typography
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import List.Extra
import Modifiers exposing (Modifier)
import OrientedLayout exposing (..)
import Process
import Task
import Time
import Types exposing (..)
import Url
import ViewHelpers exposing (..)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = renderDocument
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , coordinates = ( 0, 0 )
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


styliseNodeWithStyle : NodeWithStyle msg -> Html.Html msg
styliseNodeWithStyle ( viewWithoutStyle, styles ) =
    Html.div
        [ Html.Attributes.style "display" "grid"
        ]
        (Html.node "style"
            []
            [ Html.text
                (String.join "\n"
                    (styles |> List.Extra.unique)
                )
            ]
            :: [ viewWithoutStyle ]
        )


stylise : (a -> NodeWithStyle msg) -> a -> Html.Html msg
stylise view_ a =
    styliseNodeWithStyle (view_ a)


renderDocument : Model -> Browser.Document FrontendMsg
renderDocument model =
    { title = "WhoJam"
    , body = [ stylise view model ]
    }


view : Model -> NodeWithStyle msg
view model =
    B.div [] [ B.text "Hello world" ]
