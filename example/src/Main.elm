port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import NdArray exposing (NdArray)


type alias Data =
    { arrBuffer : Array Int
    , width : Int
    , height : Int
    }



-- Msg


type Msg
    = OnImage Data



-- MODEL


type alias Model =
    NdArray Int


init : ( Model, Cmd Msg )
init =
    ( NdArray.empty [ 0, 0, 0 ]
    , Cmd.none
    )



-- MAIN


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnImage data ->
            let
                -- Shape (width, height, RGBA)
                newModel =
                    NdArray.initialize
                        [ data.height, data.width, 4 ]
                        data.arrBuffer
            in
                ( newModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text (NdArray.toString model) ]



-- SUBSCRIPTIONS


port jsArray : (Data -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    jsArray OnImage
