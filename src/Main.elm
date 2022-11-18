module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Global exposing (adjacentSiblings, everything, global)
import Html.Styled as Html exposing (Html, a, button, div, header, input, label, li, main_, section, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, for, id, name, type_)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Task
import Time
import Time.Extra as Time exposing (Interval(..))



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , timer : Timer
    , presentations : List Presentation
    }


type Timer
    = Ready Int
    | Counting { startedAt : Time.Posix, splitTime : Int }
    | Stopped { startedAt : Time.Posix, stoppedAt : Time.Posix, splitTime : Int }


type alias Presentation =
    { title : String
    , speaker : String
    , url : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zone = Time.utc
      , time = Time.millisToPosix 0
      , timer = Ready (-5 * 1000)
      , presentations = []
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Http.get
            { url = "/presentations.json"
            , expect = Http.expectJson Loaded (Decode.list presentationDecoder)
            }
        ]
    )


presentationDecoder : Decoder Presentation
presentationDecoder =
    Decode.map3 Presentation
        (Decode.field "title" Decode.string)
        (Decode.field "speaker" Decode.string)
        (Decode.field "url" Decode.string)



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Start
    | Pause
    | Reset
    | Loaded (Result Http.Error (List Presentation))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        Start ->
            ( case model.timer of
                Ready initialTime ->
                    { model | timer = Counting { startedAt = model.time, splitTime = initialTime } }

                Stopped { startedAt, stoppedAt, splitTime } ->
                    { model | timer = Counting { startedAt = model.time, splitTime = splitTime + Time.diff Millisecond model.zone startedAt stoppedAt } }

                _ ->
                    model
            , Cmd.none
            )

        Pause ->
            ( case model.timer of
                Counting { startedAt, splitTime } ->
                    { model | timer = Stopped { startedAt = startedAt, stoppedAt = model.time, splitTime = splitTime } }

                _ ->
                    model
            , Cmd.none
            )

        Reset ->
            ( { model | timer = Ready (-5 * 1000) }, Cmd.none )

        Loaded (Ok decoded) ->
            ( { model | presentations = decoded }, Cmd.none )

        Loaded (Err _) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 200 Tick



-- VIEW


view : Model -> Html Msg
view model =
    Html.node "body"
        [ css
            [ height (vh 100)
            , property "display" "grid"
            , property "grid-template-rows" "auto 1fr"
            , backgroundColor (hsl 0 0 0.3)
            , color (hsl 0 0 1)
            ]
        ]
        [ global [ everything [ boxSizing borderBox, margin zero, padding zero ] ]
        , header [ css [ padding (em 0.5) ] ]
            [ viewDigitalClock { zone = model.zone, time = model.time } ]
        , main_
            [ css
                [ displayFlex
                , flexDirection column
                , justifyContent center
                , alignItems center
                , property "row-gap" "3em"
                ]
            ]
            [ section [] [ viewTimer model ]
            , section [] [ viewtimeTable model.presentations ]
            ]
        ]


viewDigitalClock : { a | zone : Time.Zone, time : Time.Posix } -> Html Msg
viewDigitalClock { zone, time } =
    let
        hour =
            Time.toHour zone time
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            Time.toMinute zone time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    div [ css [ fontSize (em 2), fontWeight bold ] ] [ text (hour ++ ":" ++ minute) ]


viewTimer : Model -> Html Msg
viewTimer model =
    div [ css [ displayFlex, flexDirection column, property "row-gap" "2em" ] ]
        [ viewElapsedTime model
        , viewTimerControl model
        ]


viewElapsedTime : Model -> Html Msg
viewElapsedTime { zone, time, timer } =
    div [ css [ lineHeight (num 1), fontSize (em 8), fontWeight bold ] ]
        [ text <|
            case timer of
                Ready initialTime ->
                    initialTime |> toDigitalString

                Counting { startedAt, splitTime } ->
                    (splitTime + Time.diff Millisecond zone startedAt time) |> toDigitalString

                Stopped { startedAt, stoppedAt, splitTime } ->
                    (splitTime + Time.diff Millisecond zone startedAt stoppedAt) |> toDigitalString
        ]


toDigitalString : Int -> String
toDigitalString time =
    if time >= 0 then
        toDigitalString_ time

    else
        let
            time_ =
                time |> toFloat |> (\float -> float / 1000) |> floor |> (*) 1000 |> abs
        in
        "-" ++ toDigitalString_ time_


toDigitalString_ : Int -> String
toDigitalString_ time =
    let
        hours =
            (time // 3600000)
                |> String.fromInt
                |> String.padLeft 2 '0'

        minutes =
            (modBy 3600000 time // 60000)
                |> String.fromInt
                |> String.padLeft 2 '0'

        seconds =
            (modBy 60000 time // 1000)
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    String.join ":" [ hours, minutes, seconds ]


viewTimerControl : Model -> Html Msg
viewTimerControl model =
    let
        button_ =
            Html.styled button
                [ padding2 (em 0.2) (em 0.8)
                , fontSize (em 1.2)
                , backgroundColor (hsl 0 0 0.2)
                , color (hsl 0 0 0.7)
                , borderWidth zero
                , borderRadius (em 0.3)
                ]
    in
    div [ css [ displayFlex, justifyContent center, property "column-gap" "0.5em" ] ] <|
        case model.timer of
            Ready _ ->
                [ button_ [ onClick Start ] [ text "Start" ] ]

            Counting _ ->
                [ button_ [ onClick Pause ] [ text "Pause" ] ]

            Stopped _ ->
                [ button_ [ onClick Start ] [ text "Resume" ]
                , button_ [ onClick Reset ] [ text "Reset" ]
                ]


viewtimeTable : List Presentation -> Html msg
viewtimeTable presentations =
    ul [] <|
        List.map
            (\{ title, speaker } ->
                li [ css [ listStyle none ] ]
                    [ input
                        [ id title
                        , type_ "radio"
                        , name "presentations"
                        , css
                            [ display none
                            , checked
                                [ adjacentSiblings
                                    [ Css.Global.label
                                        [ backgroundColor (hsl 0 0 0.2), color (hsl 0 0 1) ]
                                    ]
                                ]
                            ]
                        ]
                        []
                    , label
                        [ for title
                        , css
                            [ display block
                            , padding2 (em 0.8) (em 1.5)
                            , letterSpacing (em 0.01)
                            , textDecoration none
                            , color (hsl 0 0 0.45)
                            , borderRadius (em 1)
                            , cursor pointer
                            , hover [ color (hsl 0 0 0.9) ]
                            ]
                        ]
                        [ div [ css [ fontSize (em 1.2) ] ] [ text title ]
                        , div [ css [ fontSize (em 0.8) ] ] [ text speaker ]
                        ]
                    ]
            )
            presentations
