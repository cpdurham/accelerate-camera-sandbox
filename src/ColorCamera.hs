{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ColorCamera where

import Control.Monad (forever)

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO as A (fromPtr,fromVectors, toVectors)
import Data.Array.Accelerate.Data.Colour.RGB as RGB
import Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Data.Colour.HSV as HSV
import Data.Array.Accelerate.LLVM.PTX (run1)

import Pipes
import Prelude as P

import Acc.Lift (lift3E, unlift2E, unlift3E)
import Acc.Stencil
import Acc.Gaussian
import Acc.Gradient
import Acc.Transpose

type Cube a = Array DIM3 a

dim3ToTuple :: Elt a => Acc (Array DIM3 a) -> Acc (Array DIM2 (a,a,a))
dim3ToTuple arr =
  let
    (w,h,_) = unlift3E $ unindex3 $ A.shape arr
    f sh =
      let
        (x,y) = unlift2E $ unindex2 sh
        b = arr ! index3 x y 0
        g = arr ! index3 x y 1
        r = arr ! index3 x y 2
      in
        lift3E (r,g,b)
  in
    A.generate (index2 w h) f

tupToRGB :: Exp (Word8,Word8,Word8) -> Exp RGB.Colour
tupToRGB tup =
  let
    (r,g,b) = unlift3E tup
  in
    rgb8 r g b

floatTupToRGB :: Exp (Float,Float,Float) -> Exp RGB.Colour
floatTupToRGB tup =
  let
    (r,g,b) = unlift3E tup
  in
    rgb r g b

vflip :: Elt a => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
vflip arr =
  let
    (w,h) = unlift2E $ unindex2 $ A.shape arr
    f :: Exp DIM2 -> Exp DIM2
    f sh =
      let
        (x,y) = unlift2E $ unindex2 sh
      in index2 (abs $ w - x - 1) y
  in A.backpermute (index2 w h) f arr

simplePipe :: Pipe (Array DIM3 Word8) (Array DIM2 (Word8,Word8,Word8)) IO ()
simplePipe =
  forever $ do
    arr <- await
    yield $ run1 (vflip . dim3ToTuple) arr

gradientPipe :: Pipe (Array DIM3 Word8) (Array DIM2 (Word8,Word8,Word8)) IO ()
gradientPipe =
  forever $
  do
    arr <- await
    yield $ run1 (vflip . dim3ToTuple . gradientColor) arr

gradientColor :: Acc (Array DIM3 Word8) -> Acc (Array DIM3 Word8)
gradientColor arr =
  let
    sigma = 3.0
    gauss = yxStencil5x5 (gaussianSigStencil sigma)
    gradient = yxStencil3x3 gradientMagStencil
    transposed = transposeToXYZ arr
  in
    A.map toWord $ transposeToXYZ $ A.map (*4) $ gradient $ gauss $ A.map toFloat transposed

gradientLuminancePipe :: Pipe (Array DIM3 Word8) (Array DIM2 (Word8,Word8,Word8)) IO ()
gradientLuminancePipe =
  forever $
  do
    arr <- await
    yield $ run1 gradientLuminance arr

gradientLuminance :: Acc (Array DIM3 Word8) -> Acc (Array DIM2 (Word8,Word8,Word8))
gradientLuminance arr =
  vflip
--  . A.map (\tup -> let (mag,dir) = unlift2E tup in rgbToTuple . HSL.toRGB $ hsl (120+4*dir*360/45) dir mag)
  . A.map (\tup -> let (mag,dir) = unlift2E tup in rgbToTuple . HSL.toRGB $ hsl (dir*360) dir mag)
  . gradientMagDirFree
  . stencil (gaussianSigStencil 3) Clamp
  . A.map luminance . A.map tupToRGB . dim3ToTuple
  $ arr

rgbToTuple :: Exp RGB.Colour -> Exp (Word8,Word8,Word8)
rgbToTuple ec =
  let
    (RGB r g b) = unlift ec
    r' = word8OfFloat r
    g' = word8OfFloat g
    b' = word8OfFloat b
  in
    lift3E (r',g',b')

word8OfFloat :: Exp Float -> Exp Word8
word8OfFloat x = A.truncate (x * 255)

toFloat :: Exp Word8 -> Exp Float
toFloat = A.fromIntegral

toWord :: Exp Float -> Exp Word8
toWord = A.round . A.min 255
