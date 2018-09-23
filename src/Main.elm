module Main exposing (Model, Msg(..), Status(..), init, initialModel, main, renderStatus, subscriptions, thinkTime, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Task exposing (Task)
import Time exposing (Posix)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


thinkTime : Posix
thinkTime =
    Time.millisToPosix 500


hiddenMeasurementTextId =
    "hidden-measurement-text"


type Status
    = Thinking Posix
    | Empty
    | Done


type alias Model =
    { word : String
    , status : Status
    , wordSize : Maybe Float
    }


initialModel : Model
initialModel =
    { word = ""
    , status = Empty
    , wordSize = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = Tick Float
    | NewText String
    | UpdateWordSize Float
    | MissingTextElement


updateWordMeasurement =
    Task.attempt getSizeFromElement (Browser.Dom.getElement hiddenMeasurementTextId)


getSizeFromElement : Result Browser.Dom.Error Browser.Dom.Element -> Msg
getSizeFromElement res =
    case res of
        Ok el ->
            UpdateWordSize el.element.width

        Err _ ->
            MissingTextElement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewText text ->
            ( { model
                | word = text
                , status =
                    case text of
                        "" ->
                            Empty

                        _ ->
                            Thinking thinkTime
              }
            , updateWordMeasurement
            )

        Tick delta ->
            case model.word of
                "" ->
                    ( { model | status = Empty }, Cmd.none )

                _ ->
                    case model.status of
                        Thinking t ->
                            let
                                newTime =
                                    Time.millisToPosix (Time.posixToMillis t - Basics.floor delta)

                                newStatus =
                                    if Time.posixToMillis newTime > 0 then
                                        Thinking newTime

                                    else
                                        Done
                            in
                            ( { model | status = newStatus }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        UpdateWordSize size ->
            ( { model | wordSize = Just size }, Cmd.none )

        MissingTextElement ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Thinking _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none


font =
    Html.Attributes.style "font" "20px sans-serif"


view : Model -> Html Msg
view model =
    let
        inputLength =
            Maybe.withDefault 0 model.wordSize
                |> max 20
    in
    Html.div
        []
        [ Html.div
            [ Html.Attributes.style "background-color" "lightgray"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            , font
            ]
            [ Html.text "Is "
            , Html.input
                [ font
                , Html.Attributes.type_ "text"
                , Html.Attributes.value model.word
                , Html.Events.onInput NewText
                , Html.Attributes.style "margin" "0 auto"
                , Html.Attributes.style "text-align" "left"
                , Html.Attributes.style "width" (String.fromFloat inputLength ++ "px")
                ]
                []
            , Html.text " a valid Scrabble word?"
            , Html.br [] []
            , renderStatus model
            ]
        , Html.div
            [ Html.Attributes.id hiddenMeasurementTextId
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "visibility" "hidden"
            , Html.Attributes.style "height" "auto"
            , Html.Attributes.style "width" "auto"
            , Html.Attributes.style "white-space" "nowrap"
            , font
            ]
            [ Html.text model.word ]
        ]


renderStatus : Model -> Html Msg
renderStatus model =
    let
        text =
            case model.status of
                Empty ->
                    Html.text "Type a word to see if it is in the Scrabble dictionary"

                Thinking _ ->
                    Html.text "Loading..."

                Done ->
                    Html.text (model.word ++ " is a valid word!")
    in
    Html.p
        []
        [ text ]
