module Fluid.Pipe where

import Acc.Gradient
import Acc.Gaussian

import ColorCamera

import Fluid.Fluid (fluid)
import Fluid.Type

import Control.Monad (forever)

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX (run1)
import Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Data.Colour.HSV as HSV
import Data.Array.Accelerate.Data.Colour.RGB as RGB

import Pipes

fluidPipe
  :: Arrays a
  => DensityField
  -> VelocityField
  -> (Acc (a,DensityField,VelocityField) -> Acc (Pic,DensityField,VelocityField))
  -> Pipe a Pic IO ()
fluidPipe idf ivf f =
  let
    run df vf =
      do
        a <- await
        let (pic,df',vf') = run1 f (a,df,vf)
        yield pic
        run df' vf'
  in
    run idf ivf

fromArraySource
  :: Acc ((DensityField,VelocityField), DensityField, VelocityField)
  -> (Acc DensityField, Acc VelocityField)
fromArraySource input =
  let
    (sources,df,vf) = unlift input
      :: (Acc (DensityField,VelocityField), Acc DensityField, Acc VelocityField)
    (ds,vs) = unlift sources
    df' = A.zipWith (.+.) ds df
    vf' = A.zipWith (.+.) vs vf
  in
    (df',vf')

fromVectorSource
  :: Acc ((DensitySource, VelocitySource), DensityField, VelocityField)
  -> (Acc DensityField, Acc VelocityField)
fromVectorSource input =
  let
    (sources,df,vf) = unlift input
      :: (Acc (DensitySource, VelocitySource), Acc DensityField, Acc VelocityField)
    (ds,vs) = unlift sources
    df' = inject ds df
    vf' = inject vs vf
  in
    (df',vf')

fromVectorSource'
  :: Acc ((DensityField, VelocitySource), DensityField, VelocityField)
  -> (Acc DensityField, Acc VelocityField)
fromVectorSource' input =
  let
    (sources,df,vf) = unlift input
      :: (Acc (DensityField, VelocitySource), Acc DensityField, Acc VelocityField)
    (ds,vs) = unlift sources :: (Acc DensityField, Acc VelocitySource)
    df' = A.zipWith (+) ds df
    vf' = inject vs vf
  in
    (df',vf')

doublePic
  :: Pipe (Array DIM3 Word8) (Array DIM3 Word8, Array DIM3 Word8) IO ()
doublePic =
  do
    initPic <- await
    let
      run prevPic =
        do
          thisPic <- await
          yield (prevPic,thisPic)
          run thisPic
    run initPic

diffDoublePicPipe
  :: Pipe (Array DIM3 Word8, Array DIM3 Word8) Pic IO ()
diffDoublePicPipe =
  let
    f :: Acc (Array DIM3 Word8, Array DIM3 Word8) -> Acc Pic
    f tup =
      let
        (prev,curr) = A.unlift tup :: (Acc (Array DIM3 Word8), Acc (Array DIM3 Word8))
        prevF = A.map ((/255) . A.fromIntegral) prev :: Acc (Array DIM3 Float)
        currF = A.map ((/255) . A.fromIntegral) curr :: Acc (Array DIM3 Float)
        diff = A.zipWith (\a1 a2 -> abs $ A.subtract a1 a2) prevF currF :: Acc (Array DIM3 Float)
      in
        vflip . A.map rgbToTuple . A.map floatTupToRGB $ dim3ToTuple $ diff
  in
    forever $
    do
      tup <- await
      yield $ run1 f tup

fromDoublePic
  :: Acc ((Array DIM3 Word8, Array DIM3 Word8), DensityField, VelocityField)
  -> (Acc DensityField, Acc VelocityField)
fromDoublePic input =
  let
    (sources,df,vf) = unlift input :: (Acc (Array DIM3 Word8, Array DIM3 Word8), Acc DensityField, Acc VelocityField)
    (prev,curr) = A.unlift sources :: (Acc (Array DIM3 Word8), Acc (Array DIM3 Word8))
    prevF = A.map ((/255) . A.fromIntegral) $ prev :: Acc (Array DIM3 Float)
    currF = A.map ((/255) . A.fromIntegral) $ curr :: Acc (Array DIM3 Float)
    diff = A.zipWith (\a1 a2 -> abs $ A.subtract a1 a2) prevF currF :: Acc (Array DIM3 Float)
    lum = vflip . A.map luminance . A.map floatTupToRGB $ dim3ToTuple $ diff
    gradMagDir = gradientMagDirFree
                 . stencil (gaussianSigStencil 3) Clamp
                 $ lum
    vf' = A.zipWith (.+.) (A.map magDirToVec gradMagDir) vf
    df' = A.zipWith (.+.) df $ A.map (*10) lum
  in
    (df',vf')


fromCameraToLuminance
  :: Acc (Array DIM3 Word8, DensityField, VelocityField)
  -> (Acc DensityField, Acc VelocityField)
fromCameraToLuminance input =
  let
    (sources,df,vf) = unlift input
    lum = A.map magDirToVec gradMagDir
    gradMagDir = gradientMagDirFree
      . stencil (gaussianSigStencil 3) Clamp
      . A.map luminance
      . A.map tupToRGB
      . dim3ToTuple $ sources
    lumMag = A.map A.fst gradMagDir
    vf' = A.zipWith (.+.) lum vf
    df' = A.zipWith (.+.) lumMag df
  in
    (df',vf')

magDirToVec :: Exp (Float,Float) -> Exp (Float,Float)
magDirToVec tup =
  let
    (mag,dir) = unlift tup
    mag' = mag
    rad = dir*2*pi
    x = mag'*cos rad :: Exp Float
    y = mag'*sin rad :: Exp Float
  in
    A.lift (x,y)

inject
    :: FieldElt e
    => Acc (Vector Index, Vector e)
    -> Acc (Field e)
    -> Acc (Field e)
inject source field =
  let (is, ps) = A.unlift source
  in A.size ps A.== 0
       ?| ( field, A.permute (.+.) field (is A.!) ps )

colorAnglesHalloween
  :: (Acc DensityField, Acc VelocityField)
  -> Acc (Pic,DensityField,VelocityField)
colorAnglesHalloween (df,vf) =
  let pic =
        A.zipWith
        (\v tE ->
            let
              (y,x) = A.unlift tE :: (Exp Float, Exp Float)
              alpha = (A.atan2 y x - (pi/8)) * (4/pi)
              norm = (alpha + 8 * A.fromIntegral (boolToInt (alpha A.<= 0))) / 8
              mag = A.min 1 $ (sqrt $ x * x + y * y) / 0.1
            in
              rgbToTuple $ HSV.toRGB $ hsv (349+v*10) (abs $ 1 - mag) (abs $ 1-v)
        ) normDf vf
{-
      --Average value norm
      normDf =
        let
          (ydim,xdim) = A.unlift $ A.unindex2 $ A.shape df :: (Exp Int, Exp Int)
          mVal = the (A.sum df) / (A.fromIntegral $ xdim*ydim)
        in
          A.map (/mVal) df
-}

      --Max value norm
      normDf =
        let
          mVal = the $ A.maximum df
        in
          A.map (/mVal) df

  in
    A.lift (pic,df,vf)

colorAngles
  :: (Acc DensityField, Acc VelocityField)
  -> Acc (Pic,DensityField,VelocityField)
colorAngles (df,vf) =
  let pic =
        A.zipWith
        (\v tE ->
            let
              (y,x) = A.unlift tE :: (Exp Float, Exp Float)
              alpha = (A.atan2 y x - (pi/8)) * (4/pi)
              norm = (alpha + 8 * A.fromIntegral (boolToInt (alpha A.<= 0))) / 8
              mag = A.min 1 $ (sqrt $ x * x + y * y) / 0.1
            in
              rgbToTuple $ HSV.toRGB $ hsv (norm*360) mag v
        ) normDf vf
        
      --Average value norm
      normDf =
        let
          (ydim,xdim) = A.unlift $ A.unindex2 $ A.shape df :: (Exp Int, Exp Int)
          mVal = the (A.sum df) / (A.fromIntegral $ xdim*ydim)
        in
          A.map (/mVal) df
{-
      --Max value norm (looks good also)
      normDf =
        let
          mVal = the $ A.maximum df
        in
          A.map (/mVal) df
-}
  in
    A.lift (pic,df,vf)

simple
  :: (Acc DensityField, Acc VelocityField)
  -> Acc (Pic,DensityField,VelocityField)
simple (df,vf) =
  let
    pic =
      A.map
      (\x ->
          let
            c = floatToWord8 $ (255*) $ 0 `A.max` x `A.min` 1
          in
            A.lift (c,c,c)
      ) normDf
         
    normDf =
      let
        (ydim,xdim) = A.unlift $ A.unindex2 $ A.shape df :: (Exp Int, Exp Int)
        mVal = the (A.sum df) / (A.fromIntegral $ xdim*ydim)
      in
        A.map (/mVal) df

  in
    A.lift (pic,df,vf)

floatToWord8 :: Exp Float -> Exp Word8
floatToWord8 = A.truncate
