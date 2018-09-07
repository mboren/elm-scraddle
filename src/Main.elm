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
    Html.div
        [ Html.Attributes.style [ ( "background-color", "lightgray" ), ( "width", "100%" ), ( "height", "100%" ) ] ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value model.word
            , Html.Events.onInput NewText
            , Html.Attributes.style [ ( "display", "block" ), ( "margin", "0 auto" ), ( "width", "80%" ), ( "border-radius", "20px" ), ( "font-size", "300%" ), ( "text-align", "center" ) ]
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
            [ Html.Attributes.style [ ( "text-align", "center" ), ( "font-family", "sans-serif" ), ( "font-size", "200%" ) ] ]
            [ text ]
