port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import NdArray


-- Msg


type Msg
    = OnImage Model



-- MODEL


type alias Model =
    Array Int


init : ( Model, Cmd Msg )
init =
    ( Array.empty, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnImage buffer ->
            ( buffer, Cmd.none )


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (toString model) ]



-- SUBSCRIPTIONS


port jsArray : (Array Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    jsArray OnImage
