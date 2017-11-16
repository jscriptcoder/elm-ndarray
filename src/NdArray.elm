module NdArray
    exposing
        ( Location
        , Shape
        , Buffer
        , Strides
        , NdArray
        , initialize
        , toString
        , bufferToString
        , get
        , set
        , index
        , high
        , low
        , reshape
        , transpose
        , map
        , fold
        )

import Array exposing (..)
import List exposing (..)


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



{- TODO -}


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



{- TODO -}


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
        ++ "}"



{- TODO -}


bufferToString : NdArray a -> String
bufferToString nda =
    Basics.toString <| Array.toList nda.buffer



{- TODO -}


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



{- TODO -}


get : Location -> NdArray a -> Maybe a
get loc nda =
    let
        idx =
            index loc nda
    in
        Array.get idx nda.buffer



{- TODO -}


set : Location -> a -> NdArray a -> NdArray a
set loc value nda =
    let
        idx =
            index loc nda
    in
        { nda | buffer = Array.set idx value nda.buffer }



{- TODO -}


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



{- TODO -}


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



{- TODO -}


reshape : Shape -> NdArray a -> NdArray a
reshape shape nda =
    let
        strides =
            calculateStrides shape

        length =
            shapeToLength shape
    in
        { nda
            | shape = shape
            , strides = strides
            , length = length
        }



{- TODO -}


transpose : List Int -> NdArray a -> NdArray a
transpose axes nda =
    let
        arrShape =
            Array.fromList nda.shape

        arrStrides =
            Array.fromList nda.strides

        newShape =
            permuteValues axes [] arrShape

        newStrides =
            permuteValues axes [] arrStrides
    in
        { nda
            | shape = newShape
            , strides = newStrides
        }



{- TODO -}


map : (a -> b) -> NdArray a -> NdArray b
map fn nda =
    let
        bufferLength =
            Array.length nda.buffer

        newNda =
            if nda.offset > 0 || nda.length < bufferLength then
                trimBuffer nda
            else
                nda
    in
        { newNda | buffer = Array.map fn newNda.buffer }



{- TODO -}


fold : (a -> b -> b) -> b -> NdArray a -> b
fold fn initVal nda =
    let
        newNda =
            trimBuffer nda
    in
        Array.foldl fn initVal newNda.buffer



-- Helpers --


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


trimBuffer : NdArray a -> NdArray a
trimBuffer nda =
    let
        newBuffer =
            Array.slice nda.offset nda.length nda.buffer
    in
        { nda
            | buffer = newBuffer
            , offset = 0
        }


permuteValues indexes resultList arrList =
    case indexes of
        [] ->
            resultList

        idx :: tailList ->
            let
                maybeValue =
                    Array.get idx arrList
            in
                case maybeValue of
                    Just value ->
                        let
                            newResultList =
                                List.append resultList [ value ]
                        in
                            permuteValues tailList newResultList arrList

                    Nothing ->
                        permuteValues [] resultList arrList
