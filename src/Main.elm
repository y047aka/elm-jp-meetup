module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Task
import Time
import Time.Extra as Time exposing (Interval(..))



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , timer : Timer
    }


type Timer
    = Ready
    | Counting { startedAt : Time.Posix, splitTime : Int }
    | Stopped { startedAt : Time.Posix, stoppedAt : Time.Posix, splitTime : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zone = Time.utc
      , time = Time.millisToPosix 0
      , timer = Ready
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Start
    | Pause
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        Start ->
            ( case model.timer of
                Ready ->
                    { model | timer = Counting { startedAt = model.time, splitTime = 0 } }

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
            ( { model | timer = Ready }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 200 Tick



-- VIEW


view : Model -> Html Msg
view model =
    Html.node "body"
        []
        [ viewDigitalClock { zone = model.zone, time = model.time }
        , viewTimer model
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
    text (hour ++ ":" ++ minute)


viewTimer : Model -> Html Msg
viewTimer model =
    div []
        [ viewElapsedTime model
        , viewTimerControl model
        ]


viewElapsedTime : Model -> Html Msg
viewElapsedTime { zone, time, timer } =
    div []
        [ text <|
            case timer of
                Ready ->
                    "00:00:00"

                Counting { startedAt, splitTime } ->
                    (splitTime + Time.diff Millisecond zone startedAt time) |> toDigitalString

                Stopped { startedAt, stoppedAt, splitTime } ->
                    (splitTime + Time.diff Millisecond zone startedAt stoppedAt) |> toDigitalString
        ]


toDigitalString : Int -> String
toDigitalString time =
    let
        absolute =
            abs time

        sign =
            if time < 0 then
                "-"

            else
                ""

        hours =
            (absolute // 3600000)
                |> String.fromInt
                |> String.padLeft 2 '0'

        minutes =
            (modBy 3600000 absolute // 60000)
                |> String.fromInt
                |> String.padLeft 2 '0'

        seconds =
            (modBy 60000 absolute // 1000)
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    sign ++ String.join ":" [ hours, minutes, seconds ]


viewTimerControl : Model -> Html Msg
viewTimerControl model =
    div [] <|
        case model.timer of
            Ready ->
                [ button [ onClick Start ] [ text "Start" ] ]

            Counting _ ->
                [ button [ onClick Pause ] [ text "Pause" ] ]

            Stopped _ ->
                [ button [ onClick Start ] [ text "Resume" ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]
