module Main exposing (Model, Msg(..), Status(..), init, initialModel, main, renderStatus, subscriptions, thinkTime, update, view)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
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


type Status
    = Thinking Posix
    | Empty
    | Done


type alias Model =
    { word : String
    , status : Status
    }


initialModel : Model
initialModel =
    { word = ""
    , status = Empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = Tick Float
    | NewText String


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
            , Cmd.none
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Thinking _ ->
            Browser.Events.onAnimationFrameDelta Tick

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "background-color" "lightgray", Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%" ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value model.word
            , Html.Events.onInput NewText
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "margin" "0 auto"
            , Html.Attributes.style "width" "80%"
            , Html.Attributes.style "border-radius" "20px"
            , Html.Attributes.style "font-size" "300%"
            , Html.Attributes.style "text-align" "center"
            ]
            []
        , Html.br [] []
        , renderStatus model
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
        [ Html.Attributes.style "text-align" "center", Html.Attributes.style "font-family" "sans-serif", Html.Attributes.style "font-size" "200%" ]
        [ text ]
