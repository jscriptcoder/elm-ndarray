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
    , offset : Int
    , strides : Strides
    }


initialize : Shape -> Buffer a -> NdArray a
initialize shape buffer =
    let
        strides =
            calculateStrides shape

        offset =
            0
    in
        { shape = shape
        , strides = strides
        , offset = offset
        , buffer = buffer
        }


toString : NdArray a -> String
toString nda =
    "NdArray{shape="
        ++ (Basics.toString nda.shape)
        ++ ";strides="
        ++ (Basics.toString nda.strides)
        ++ ";offset="
        ++ (Basics.toString nda.offset)


bufferToString : NdArray a -> String
bufferToString nda =
    Basics.toString <| Array.toList nda.buffer


get : Location -> NdArray a -> Maybe a
get location nda =
    let
        idx =
            index location nda
    in
        Array.get idx nda.buffer


set : Location -> a -> NdArray a -> NdArray a
set location value nda =
    let
        idx =
            index location nda
    in
        if idx >= 0 then
            { nda
                | buffer = Array.set idx value nda.buffer
            }
        else
            nda


index : Location -> NdArray a -> Int
index location nda =
    let
        buffLen =
            Array.length nda.buffer
    in
        let
            locTimesStride =
                List.map2
                    (\loc stride -> loc * stride)
                    location
                    nda.strides

            idx =
                (List.foldl (+) 0 locTimesStride) + nda.offset
        in
            if idx < buffLen then
                idx
            else
                -1


map : (a -> b) -> NdArray a -> NdArray b
map fn nda =
    { nda
        | buffer = Array.map fn nda.buffer
    }


reduce : (a -> b -> b) -> b -> NdArray a -> b
reduce fn initVal nda =
    Array.foldl fn initVal nda.buffer


high : Location -> NdArray a -> NdArray a
high location nda =
    let
        newShape =
            location
    in
        { nda
            | shape = newShape
            , strides = calculateStrides newShape
            , offset = nda.offset
        }


low : Location -> NdArray a -> NdArray a
low location nda =
    { nda
        | shape = nda.shape
        , strides = nda.strides
        , offset = nda.offset
    }



{-
   transpose : List Int -> NdArray a -> NdArray a
   transpose shapeIdx nda =
       { nda
           | shape = []
           , strides = []
       }
-}


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
