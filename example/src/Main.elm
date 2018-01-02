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
    | Process



-- MODEL


type alias Model =
    NdArray Int


init : ( Model, Cmd Msg )
init =
    ( NdArray.empty
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

        Process ->
            let
                highNda =
                    NdArray.high [ 200, 200, 4 ] model

                lowNda =
                    NdArray.low [ 100, 100, 0 ] model

                viewNda =
                    NdArray.view lowNda

                arrBuffer =
                    viewNda.buffer
            in
                ( model
                , elm2js <|
                    { arrBuffer = arrBuffer
                    , width = 173
                    , height = 173
                    }
                )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (NdArray.toString model) ]
        , button [ onClick Process ] [ text "Process" ]
        ]



-- SUBSCRIPTIONS


port js2elm : (Data -> msg) -> Sub msg


port elm2js : Data -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    js2elm OnImage
