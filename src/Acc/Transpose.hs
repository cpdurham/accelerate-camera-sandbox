module Acc.Transpose where

import Data.Array.Accelerate as A

import Acc.Lift (unlift3E)

transposeToZXY
  :: Elt a
  => Acc (Array DIM3 a)
  -> Acc (Array DIM3 a)
transposeToZXY arr =
  let
    dim' =
      let
        (z,y,x) = unlift3E $ unindex3 $ A.shape arr
      in
        index3 z x y
    f :: Exp DIM3 -> Exp DIM3
    f sh =
      let
        (z,y,x) = unlift3E $ unindex3 sh
      in
        index3 z x y
  in
    backpermute dim' f arr

transposeToXYZ
  :: Elt a
  => Acc (Array DIM3 a)
  -> Acc (Array DIM3 a)
transposeToXYZ arr =
  let
    dim' =
      let
        (z,y,x) = unlift3E $ unindex3 $ A.shape arr
      in
        index3 x y z

    f :: Exp DIM3 -> Exp DIM3
    f sh =
      let
        (z,y,x) = unlift3E $ unindex3 sh
      in
        index3 x y z
  in
    backpermute dim' f arr
