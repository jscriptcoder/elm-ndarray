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
                        |> Expect.equal "NdArray{shape=[2,3];strides=[3,1];length=6;offset=0}"
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
        , test "Wrong location"
            (\_ ->
                let
                    buffer =
                        Array.initialize 12 identity

                    nda =
                        NdArray.initialize [ 3, 2, 2 ] buffer
                in
                    NdArray.index [ 5, 10, 15 ] nda
                        |> Expect.equal -1
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
        , test "Slicing high"
            (\_ ->
                let
                    buffer =
                        Array.initialize 16 identity

                    nda =
                        NdArray.initialize [ 4, 4 ] buffer

                    ndaHigh =
                        NdArray.high [ 2, 3 ] nda
                in
                    NdArray.toString ndaHigh
                        |> Expect.equal "NdArray{shape=[2,3];strides=[3,1];length=6;offset=0}"
            )
        , test "Slicing low"
            (\_ ->
                let
                    buffer =
                        Array.initialize 16 identity

                    nda =
                        NdArray.initialize [ 4, 4 ] buffer

                    ndaLow =
                        NdArray.low [ 2, 3 ] nda
                in
                    NdArray.toString ndaLow
                        |> Expect.equal "NdArray{shape=[2,1];strides=[1,1];length=2;offset=11}"
            )
        , test "Reshaping"
            (\_ ->
                let
                    buffer =
                        Array.initialize 16 identity

                    nda =
                        NdArray.initialize [ 4, 4 ] buffer

                    reshapedNda =
                        NdArray.reshape [ 2, 2, 4 ] nda
                in
                    NdArray.toString reshapedNda
                        |> Expect.equal "NdArray{shape=[2,2,4];strides=[8,4,1];length=16;offset=0}"
            )
        , test "Transposing"
            (\_ ->
                let
                    buffer =
                        Array.initialize 24 identity

                    nda =
                        NdArray.initialize [ 2, 3, 4 ] buffer

                    transposedNda =
                        NdArray.transpose [ 0, 2, 1 ] nda
                in
                    NdArray.toString transposedNda
                        |> Expect.equal "NdArray{shape=[2,4,3];strides=[12,1,4];length=24;offset=0}"
            )
        ]
