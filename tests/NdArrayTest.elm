module NdArrayTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Array exposing (..)
import NdArray exposing (..)


suite : Test
suite =
    describe "Testing NdArray API"
        [ test "Initialization"
            (\_ ->
                let
                    buffer =
                        Array.initialize 6 identity

                    nda =
                        NdArray.initialize [ 2, 3 ] buffer
                in
                    NdArray.toString nda
                        |> Expect.equal "NdArray{shape=[2,3];strides=[3,1];offset=0"
            )
        , test "Getter"
            (\_ ->
                let
                    buffer =
                        Array.initialize 6 identity

                    nda =
                        NdArray.initialize [ 3, 2 ] buffer

                    maybeVal =
                        NdArray.get [ 1, 1 ] nda
                in
                    case maybeVal of
                        Just val ->
                            Expect.equal val 3

                        Nothing ->
                            Expect.fail "Should not happen"
            )
        , test "Getter returns nothing"
            (\_ ->
                let
                    buffer =
                        Array.initialize 6 identity

                    nda =
                        NdArray.initialize [ 3, 2 ] buffer

                    maybeVal =
                        NdArray.get [ 10, 4 ] nda
                in
                    case maybeVal of
                        Just val ->
                            Expect.fail "Should not happen"

                        Nothing ->
                            Expect.pass
            )
        , test "Setter"
            (\_ ->
                let
                    buffer =
                        Array.initialize 6 identity

                    nda =
                        NdArray.initialize [ 3, 2 ] buffer

                    newNda =
                        NdArray.set [ 1, 1 ] -8 nda
                in
                    NdArray.bufferToString newNda
                        |> Expect.equal "[0,1,2,-8,4,5]"
            )
        , test "Nothing is set"
            (\_ ->
                let
                    buffer =
                        Array.initialize 6 identity

                    nda =
                        NdArray.initialize [ 2, 3 ] buffer

                    newNda =
                        NdArray.set [ 10, 4 ] 15 nda
                in
                    NdArray.bufferToString newNda
                        |> Expect.equal "[0,1,2,3,4,5]"
            )
        , test "Indexing"
            (\_ ->
                let
                    buffer =
                        Array.initialize 12 identity

                    nda =
                        NdArray.initialize [ 3, 2, 2 ] buffer
                in
                    NdArray.index [ 2, 0, 1 ] nda
                        |> Expect.equal 9
            )
        ]
