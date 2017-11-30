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


dataToString : NdArray a -> String
dataToString nda =
    Debug.crash "TODO"



{-
   function View3darray_index(i0, i1, i2) {
     return this.offset + this.stride[0] * i0 + this.stride[1] * i1 + this.stride[2] * i2;
   }
-}


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



{-
   function View3darray_get(i0, i1, i2) {
     var idx = View3darray_index(i0, i1, i2);
     return this.data[idx];
   }
-}


get : Location -> NdArray a -> Maybe a
get loc nda =
    let
        idx =
            index loc nda
    in
        Array.get idx nda.buffer



{-
   function View3darray_set(i0, i1, i2, v) {
     var idx = View3darray_index(i0, i1, i2),
     return this.data[idx] = v;
   }
-}


set : Location -> a -> NdArray a -> NdArray a
set loc value nda =
    let
        idx =
            index loc nda
    in
        { nda | buffer = Array.set idx value nda.buffer }



{-
   function View3darray_hi(i0, i1, i2) {
       i0 = (typeof i0 !== 'number' || i0 < 0) ? this.shape[0] : i0 | 0;
       i1 = (typeof i1 !== 'number' || i1 < 0) ? this.shape[1] : i1 | 0;
       i2 = (typeof i2 !== 'number' || i2 < 0) ? this.shape[2] : i2 | 0;

       return new View3darray(
           this.data,
           i0,
           i1,
           i2,
           this.stride[0],
           this.stride[1],
           this.stride[2],
           this.offset
       );
   }
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



{-
   function View3darray_lo(i0, i1, i2) {
       var b = this.offset,
           d = 0,
           a0 = this.shape[0],
           a1 = this.shape[1],
           a2 = this.shape[2],
           c0 = this.stride[0],
           c1 = this.stride[1],
           c2 = this.stride[2];

       if (typeof i0 === 'number' && i0 >= 0) {
           d = i0 | 0;
           b += c0 * d;
           a0 -= d;
       }

       if (typeof i1 === 'number' && i1 >= 0) {
           d = i1 | 0;
           b += c1 * d;
           a1 -= d;
       }

       if (typeof i2 === 'number' && i2 >= 0) {
           d = i2 | 0;
           b += c2 * d;
           a2 -= d;
       }

       return new View3darray(
         this.data,
         a0,
         a1,
         a2,
         c0,
         c1,
         c2,
         b
       )
   }
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



{-
   function View3darray_step(i0, i1, i2) {
       var a0 = this.shape[0],
           a1 = this.shape[1],
           a2 = this.shape[2],
           b0 = this.stride[0],
           b1 = this.stride[1],
           b2 = this.stride[2],
           c = this.offset,
           d = 0,
           ceil = Math.ceil;

       if (typeof i0 === 'number') {
           d = i0 | 0;
           if (d < 0) {
               c += b0 * (a0 - 1);
               a0 = ceil(-a0 / d)
           } else {
               a0 = ceil(a0 / d)
           }
           b0 *= d
       }

       if (typeof i1 === 'number') {
           d = i1 | 0;
           if (d < 0) {
               c += b1 * (a1 - 1);
               a1 = ceil(-a1 / d)
           } else {
               a1 = ceil(a1 / d)
           }
           b1 *= d
       }

       if (typeof i2 === 'number') {
           d = i2 | 0;
           if (d < 0) {
               c += b2 * (a2 - 1);
               a2 = ceil(-a2 / d)
           } else {
               a2 = ceil(a2 / d)
           }
           b2 *= d
       }

       return new View3darray(this.data, a0, a1, a2, b0, b1, b2, c);
   }
-}


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



{-
   function View3darray_pick(i0, i1, i2) {
       var a = [],
           b = [],
           c = this.offset;

       if (typeof i0 === 'number' && i0 >= 0) {
           c = (c + this.stride[0] * i0) | 0
       } else {
           a.push(this.shape[0]);
           b.push(this.stride[0]);
       }

       if (typeof i1 === 'number' && i1 >= 0) {
           c = (c + this.stride[1] * i1) | 0
       } else {
           a.push(this.shape[1]);
           b.push(this.stride[1]);
       }

       if (typeof i2 === 'number' && i2 >= 0) {
           c = (c + this.stride[2] * i2) | 0
       } else {
           a.push(this.shape[2]);
           b.push(this.stride[2]);
       }

       var ctor = CTOR_LIST[a.length + 1];
       return ctor(this.data, a, b, c);
   }
-}


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



{-
   function View3darray_transpose(i0, i1, i2) {
       i0 = (i0 === undefined ? 0 : i0 | 0);
       i1 = (i1 === undefined ? 1 : i1 | 0);
       i2 = (i2 === undefined ? 2 : i2 | 0);

       var a = this.shape,
           b = this.stride;

       return new View3darray(
         this.data,
         a[i0],
         a[i1],
         a[i2],
         b[i0],
         b[i1],
         b[i2],
         this.offset
       )
   }
-}


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
    Debug.crash "TODO"



{- TODO -}


fold : (a -> b -> b) -> b -> NdArray a -> b
fold fn initVal nda =
    Debug.crash "TODO"



-- Helpers --
{-
   stride = new Array(d)
   for(var i=d-1, sz=1; i>=0; --i) {
     stride[i] = sz
     sz *= shape[i]
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
