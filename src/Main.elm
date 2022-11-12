module Main exposing (main)

import Browser
import Html exposing (Html)



-- MAIN


main : Program { height : Int, width : Int } Model Msg
main =
    Browser.element
        { init = \flags -> ( init flags, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { window : { width : Int, height : Int } }


init : { width : Int, height : Int } -> Model
init window =
    { window = window }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW


view : Model -> Html Msg
view model =
    Html.text "hello"
