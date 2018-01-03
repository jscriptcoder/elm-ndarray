module NdArray
    exposing
        ( --Types and constructors
          Location
        , Shape
        , Buffer
        , Strides
        , NdArray
          -- Creating
        , initialize
        , empty
          -- Stringifying
        , toString
        , bufferToString
        , viewToString
          -- Reading and writing
        , index
        , get
        , set
          -- Slicing
        , high
        , low
        , step
        , pick
          -- Reshaping
        , reshape
        , transpose
        , map
        , fold
        , view
        )

{-| A NdArray provides a higher dimensional views of 1D arrays. This is a port
of [scijs/ndarray](https://github.com/scijs/ndarray) by Mikola Lysenko.


# Types and constructors

@docs Location, Shape, Strides, Buffer, NdArray


# Creating

@docs initialize, empty


# Stringifying

@docs toString, bufferToString


# Reading and writing

@docs index, get, set


# Slicing

-}

import Array exposing (..)
import List exposing (..)


{-| List of Int representing the location within the [NdArray](#NdArray).

    let
        nda =
            initialize [2, 3] [1, 2, 3, 4, 5, 6]

        location =
            [1, 0]
    in
        get location nda == 3

-}
type alias Location =
    List Int


{-| List of Int representing the dimensions of the [NdArray](#NdArray).

    [4] -- 1D vector
    [3, 4] -- 3×4 matrix
    [2, 1, 3] -- 3 dimensional matrix, 2×1×3

-}
type alias Shape =
    List Int


{-| List of Int representing the stride for each dimension.

    let
        nda =
            initialize [2, 2, 2] [1, 2, 3, 4, 5, 6, 7, 8]
    in
        nda.strides == [4, 2, 1]

-}
type alias Strides =
    List Int


{-| Underlying one dimensional array
TODO: TypedArray?
-}
type alias Buffer a =
    Array a


{-| Multidimensional container of items of the same type.
The number of dimensions and items in an NdArray is defined by its
[Shape](#Shape)
-}
type alias NdArray a =
    { shape : Shape
    , buffer : Buffer a
    , length : Int
    , offset : Int
    , strides : Strides
    }


{-| Initialize a [NdArray](#NdArray) given a [Shape](#Shape) and a [Buffer](#Buffer).
By default the offset to start the view is 0. Strides and length are
calculated based on the [Shape](#Shape)

    initialize [2, 3] [1, 2, 3, 4, 5, 6]
    --> { shape = [2, 3]
        , buffer = [1, 2, 3, 4, 5, 6]
        , length = 6
        , offset = 0
        , strides = [3, 1]
        }

-}
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


{-| Creates an empty [NdArray](#NdArray).

    empty
    --> { shape = []
        , buffer = []
        , length = 0
        , offset = 0
        , strides = []
        }

-}
empty : NdArray a
empty =
    initialize [] Array.empty


{-| Turns a [NdArray](#NdArray) into a string. The [Buffer](#Buffer) is left out.

    let
        nda = initialize [2, 3][1, 2, 3, 4, 5, 6]

    in
        toString nda
        --> "NdArray{shape=[2,3];strides=[3,1];length=6;offset=0}"

-}
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


{-| String representation of the [Buffer](#Buffer). For debugging purposes.

    let
        nda = initialize [2, 3][1, 2, 3, 4, 5, 6]

    in
        bufferToString nda
        --> "[1,2,3,4,5,6]"

-}
bufferToString : NdArray a -> String
bufferToString nda =
    Basics.toString <| Array.toList nda.buffer


{-| String representation of the view [Buffer](#Buffer). For debugging purposes.
-}
viewToString : NdArray a -> String
viewToString nda =
    let
        viewNda =
            view nda
    in
        bufferToString viewNda


{-| Returns the index of the cell in the underlying [NdArray](#NdArray).

    let
        nda =
            initialize [2, 3] [1, 2, 3, 4, 5, 6]

        location =
            [1, 0]
    in
        index location nda == 2

-}
index : Location -> NdArray a -> Int
index loc nda =
    let
        locTimesStride =
            List.map2
                (\loc stride -> loc * stride)
                loc
                nda.strides
    in
        List.foldl (+) nda.offset locTimesStride


{-| Returns an element given the [Location](#Location).

    let
        nda =
            initialize [2, 3] [1, 2, 3, 4, 5, 6]

        location =
            [1, 1]
    in
        get location nda == Just 4

-}
get : Location -> NdArray a -> Maybe a
get loc nda =
    let
        idx =
            index loc nda
    in
        Array.get idx nda.buffer


{-| Sets an element given the [Location](#Location).

    let
        nda =
            initialize [2, 3] [1, 2, 3, 4, 5, 6]

        location =
            [1, 1]
    in
        set location -1 nda |> bufferToString
        --> "[1,2,3,-1,5,6]"

-}
set : Location -> a -> NdArray a -> NdArray a
set loc value nda =
    let
        idx =
            index loc nda
    in
        { nda | buffer = Array.set idx value nda.buffer }


{-| This creates a shifted view of the array, truncating from the bottom-right
corner of the [NdArray](#NdArray).

    let
        nda = initialize [3, 4] [1, 2, ..., 12]
        {-
            1  2  3  4
            5  6  7  8
            9 10 11 12
        -}

    in
        high [2, 3] nda
        {-
            1  2  3
            5  6  7
        -}

-}
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

        newLength =
            shapeToLength newShape
    in
        { nda
            | shape = newShape
            , length = newLength
        }


{-| Does dual of high, adding offset and therefore truncating from the top-left
of the [NdArray](#NdArray).

    let
        nda = initialize [3, 4] [1, 2, ..., 12]
        {-
            1  2  3  4
            5  6  7  8
            9 10 11 12
        -}

    in
        low [2, 2] nda
        {-
             7  8
            11 12
        -}

-}
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

        newLength =
            shapeToLength newShape
    in
        { nda
            | shape = newShape
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



{- TODO -}


view : NdArray a -> NdArray a
view nda =
    map Basics.identity nda



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
    case shape of
        [] ->
            0

        _ ->
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
