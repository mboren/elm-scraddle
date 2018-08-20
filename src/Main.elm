module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Time exposing (Time)
import AnimationFrame


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


thinkTime : Time
thinkTime =
    0.5 * Time.second


type Status
    = Thinking Time
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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


type Msg
    = Tick Time
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
                                    t - delta

                                newStatus =
                                    if newTime > 0 then
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
            AnimationFrame.diffs Tick

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value model.word
            , Html.Events.onInput NewText
            ]
            []
        , Html.br [] []
        , renderStatus model
        ]


renderStatus : Model -> Html Msg
renderStatus model =
    case model.status of
        Empty ->
            Html.text ""

        Thinking _ ->
            Html.text "Loading..."

        Done ->
            Html.text (model.word ++ " is a valid Scrabble word!")
