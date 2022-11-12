module Main exposing (main)

import Browser
import Html exposing (Html)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { count : Int }


init : Model
init =
    { count = 0 }



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
