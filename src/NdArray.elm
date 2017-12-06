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
        , viewToString
        , get
        , set
        , index
        , high
        , low
        , step
        , pick
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


viewToString : NdArray a -> String
viewToString nda =
    fold (\val acc -> val :: acc) [] nda
        |> List.reverse
        |> Basics.toString



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
            (List.foldl (+) nda.offset locTimesStride)
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
            List.map2
                (\idx dim ->
                    if idx < 0 then
                        dim
                    else
                        idx
                )
                loc
                nda.shape

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
                (\idx dim ->
                    if idx < 0 then
                        dim
                    else
                        dim - idx
                )
                loc
                nda.shape

        locTimesStride =
            List.map2
                (\idx stride ->
                    if idx < 0 then
                        0
                    else
                        idx * stride
                )
                loc
                nda.strides

        newOffset =
            List.foldl
                (\ls acc -> acc + ls)
                nda.offset
                locTimesStride

        newStrides =
            calculateStrides newShape

        newLength =
            shapeToLength newShape
    in
        { nda
            | shape = newShape
            , strides = newStrides
            , length = newLength
            , offset = newOffset
        }



{- TODO -}


step : List Int -> NdArray a -> NdArray a
step steps nda =
    let
        ( newShape, newStrides ) =
            List.map3
                (\step stride dim ->
                    let
                        newStride =
                            stride * step

                        fStep =
                            Basics.toFloat step

                        fDim =
                            Basics.toFloat dim
                    in
                        if step < 0 then
                            ( Basics.ceiling (-fDim / fStep), newStride )
                        else
                            ( Basics.ceiling (fDim / fStep), newStride )
                )
                steps
                nda.strides
                nda.shape
                |> List.unzip

        sumOffset =
            List.map3
                (\step stride dim ->
                    if step < 0 then
                        nda.offset + stride * (dim - 1)
                    else
                        nda.offset
                )
                steps
                nda.strides
                nda.shape

        newOffset =
            List.foldl (+) nda.offset sumOffset

        newLength =
            shapeToLength newShape
    in
        { nda
            | shape = newShape
            , strides = newStrides
            , length = newLength
            , offset = newOffset
        }



{- TODO -}


pick : List (Maybe Int) -> NdArray a -> NdArray a
pick picks nda =
    let
        ( auxShape, auxStrides ) =
            List.map3
                (\pick stride dim ->
                    case pick of
                        Just dim ->
                            ( 0, 0 )

                        Nothing ->
                            ( dim, stride )
                )
                picks
                nda.strides
                nda.shape
                |> List.unzip

        newShape =
            List.filter (\dim -> dim /= 0) auxShape

        newStrides =
            List.filter (\stride -> stride /= 0) auxStrides

        newLength =
            shapeToLength newShape

        sumOffset =
            List.map2
                (\pick stride ->
                    case pick of
                        Just pick ->
                            nda.offset + stride * pick

                        Nothing ->
                            0
                )
                picks
                nda.strides

        newOffset =
            List.foldl (+) nda.offset sumOffset
    in
        { nda
            | shape = newShape
            , strides = newStrides
            , length = newLength
            , offset = newOffset
        }



{- TODO -}


reshape : Shape -> NdArray a -> NdArray a
reshape newShape nda =
    let
        newStrides =
            calculateStrides newShape

        newLength =
            shapeToLength newShape
    in
        { nda
            | shape = newShape
            , strides = newStrides
            , length = newLength
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
        ndim =
            List.length nda.shape

        initialLoc =
            List.repeat ndim 0

        buffer =
            Array.empty
    in
        mapWithLocation fn initialLoc buffer nda



{- TODO -}


fold : (a -> b -> b) -> b -> NdArray a -> b
fold fn initVal nda =
    let
        ndim =
            List.length nda.shape

        initialLoc =
            List.repeat ndim 0
    in
        foldWithLocation fn initVal initialLoc nda



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


permuteValues indexes resultList arrList =
    case indexes of
        [] ->
            List.reverse resultList

        idx :: tailList ->
            let
                maybeValue =
                    Array.get idx arrList
            in
                case maybeValue of
                    Just value ->
                        let
                            newResultList =
                                value :: resultList
                        in
                            permuteValues tailList newResultList arrList

                    Nothing ->
                        permuteValues [] resultList arrList


nextLocation : Location -> Shape -> Maybe Location
nextLocation loc shape =
    let
        increment : ( Int, Int ) -> ( Location, Bool ) -> ( Location, Bool )
        increment ( idx, dim ) ( locAcc, shouldInc ) =
            if shouldInc then
                if idx + 1 < dim then
                    ( idx + 1 :: locAcc, False )
                else
                    ( 0 :: locAcc, True )
            else
                ( idx :: locAcc, False )

        newLoc =
            List.map2 (,) loc shape
                |> List.foldr increment ( [], True )
    in
        if Tuple.second newLoc then
            Nothing
        else
            Just (Tuple.first newLoc)


foldWithLocation : (a -> b -> b) -> b -> Location -> NdArray a -> b
foldWithLocation fn acc loc nda =
    let
        maybeVal =
            get loc nda
    in
        case maybeVal of
            Just val ->
                let
                    newVal =
                        fn val acc

                    maybeNextLoc =
                        nextLocation loc nda.shape
                in
                    case maybeNextLoc of
                        Just nextLoc ->
                            foldWithLocation fn newVal nextLoc nda

                        Nothing ->
                            newVal

            Nothing ->
                acc


mapWithLocation : (a -> b) -> Location -> Buffer b -> NdArray a -> NdArray b
mapWithLocation fn loc buffer nda =
    let
        maybeVal =
            get loc nda
    in
        case maybeVal of
            Just val ->
                let
                    newVal =
                        fn val

                    newBuffer =
                        Array.push newVal buffer

                    maybeNextLoc =
                        nextLocation loc nda.shape
                in
                    case maybeNextLoc of
                        Just nextLoc ->
                            mapWithLocation fn nextLoc newBuffer nda

                        Nothing ->
                            initialize nda.shape newBuffer

            Nothing ->
                initialize nda.shape buffer
