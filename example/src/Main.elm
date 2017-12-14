module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing (Value)
import Task
import FileReader exposing (NativeFile, FileContentArrayBuffer)


type alias Model =
    { file : Maybe NativeFile
    , content : Maybe FileContentArrayBuffer
    }


init : Model
init =
    { file = Nothing
    , content = Nothing
    }


type Msg
    = OnFileContent (Result FileReader.Error FileContentArrayBuffer)
    | FileChange (List NativeFile)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnFileContent res ->
            case res of
                Ok content ->
                    ( { model | content = Just content }, Cmd.none )

                Err err ->
                    Debug.crash (toString err)

        FileChange file ->
            case file of
                -- Only handling case of a single file
                [ f ] ->
                    ( { model | file = Just f }, getFileContents f )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "panel" ] <|
        [ h1 [] [ text "File Reader library example" ]
        , p [] [ text "Use the file dialog to load file" ]
        , div []
            [ input
                [ type_ "file"
                , FileReader.onFileChange FileChange
                , multiple False
                ]
                []
            ]
        , case model.file of
            Just nf ->
                div []
                    [ h3 [] [ text nf.name ]
                    , textarea
                        []
                        [ case model.content of
                            Just cf ->
                                cf
                                    |> toString
                                    |> text

                            Nothing ->
                                text ""
                        ]
                    ]

            Nothing ->
                text ""
        ]



--


getFileContents : NativeFile -> Cmd Msg
getFileContents nf =
    FileReader.readAsArrayBuffer nf.blob
        |> Task.attempt OnFileContent



--


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
