module NdArray exposing (..)

import List exposing (..)
import Array exposing (..)


type alias Location =
    List Int


type alias Shape =
    List Int


type alias Buffer a =
    Array a


type alias Strides =
    List Int


type alias NdArray a =
    { shape : Shape
    , buffer : Buffer a
    , length : Int
    , offset : Int
    , strides : Strides
    }


initialize : Shape -> Buffer a -> NdArray a
initialize shape buffer =
    let
        strides =
            calculateStrides shape

        length =
            shapeToLength shape

        offset =
            0
    in
        { shape = shape
        , strides = strides
        , length = length
        , offset = offset
        , buffer = buffer
        }


toString : NdArray a -> String
toString nda =
    "NdArray{shape="
        ++ (Basics.toString nda.shape)
        ++ ";strides="
        ++ (Basics.toString nda.strides)
        ++ ";length="
        ++ (Basics.toString nda.length)
        ++ ";offset="
        ++ (Basics.toString nda.offset)


bufferToString : NdArray a -> String
bufferToString nda =
    Basics.toString <| Array.toList nda.buffer


get : Location -> NdArray a -> Maybe a
get loc nda =
    let
        idx =
            index loc nda
    in
        Array.get idx nda.buffer


set : Location -> a -> NdArray a -> NdArray a
set loc value nda =
    let
        idx =
            index loc nda
    in
        if idx >= 0 then
            { nda
                | buffer = Array.set idx value nda.buffer
            }
        else
            nda


index : Location -> NdArray a -> Int
index loc nda =
    let
        locTimesStride =
            List.map2
                (\loc stride -> loc * stride)
                loc
                nda.strides

        idx =
            (List.foldl (+) 0 locTimesStride) + nda.offset
    in
        if idx < nda.length then
            idx
        else
            -1


high : Location -> NdArray a -> NdArray a
high loc nda =
    let
        newShape =
            loc

        newStrides =
            calculateStrides newShape

        newLength =
            shapeToLength newShape
    in
        { nda
            | shape = newShape
            , strides = newStrides
            , length = newLength
            , offset = nda.offset
        }


low : Location -> NdArray a -> NdArray a
low loc nda =
    let
        newShape =
            List.map2
                (\loc dim -> dim - loc)
                loc
                nda.shape

        newStrides =
            calculateStrides newShape

        newLength =
            shapeToLength newShape

        locTimesStride =
            List.map2
                (\loc stride -> loc * stride)
                loc
                nda.strides

        newOffset =
            List.foldl
                (\ls acc -> acc + ls)
                nda.offset
                locTimesStride
    in
        { nda
            | shape = newShape
            , strides = newStrides
            , length = newLength
            , offset = newOffset
        }


map : (a -> b) -> NdArray a -> NdArray b
map fn nda =
    { nda
        | buffer = Array.map fn nda.buffer
    }


foldl : (a -> b -> b) -> b -> NdArray a -> b
foldl fn initVal nda =
    Array.foldl fn initVal nda.buffer


calculateStrides : Shape -> Strides
calculateStrides shape =
    let
        acc =
            List.foldr
                (\dim acc ->
                    { acc
                        | strides = acc.size :: acc.strides
                        , size = acc.size * dim
                    }
                )
                { strides = []
                , size = 1
                }
                shape
    in
        acc.strides


shapeToLength : Shape -> Int
shapeToLength shape =
    List.foldr (*) 1 shape
