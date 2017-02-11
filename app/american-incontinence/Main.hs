{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.Data.Colour.RGB as RGB
import Data.Array.Accelerate.Data.Colour.Names

import Data.Vector.Storable as S

import Prelude as P

import Pipes
import qualified Pipes.Prelude as Pipes

import System.Directory
import System.Environment
import System.Exit
import System.Random.Mersenne

import Text.Printf

import Acc.Lift as A

import Fluid.Pipe
import Fluid.Fluid
import Fluid.Type

picLocation :: FilePath
picLocation = "app/american-incontinence/trump_profile_big.bmp"

maskLocation :: FilePath
maskLocation = "app/american-incontinence/trump_profile_big_mask.bmp"

noseMaskLocation :: FilePath
noseMaskLocation = "app/american-incontinence/trump_profile_big_mask_nose.bmp"

mouthMaskLocation :: FilePath
mouthMaskLocation = "app/american-incontinence/trump_profile_big_mask_mouth.bmp"

eyeMaskLocation :: FilePath
eyeMaskLocation = "app/american-incontinence/trump_profile_big_mask_eye.bmp"

newtype ColorState = ColorState (Int,Int)
data SimState = SimState ColorState ColorState ColorState

initialState :: SimState
initialState = SimState initialColorState initialColorState initialColorState
  where initialColorState = ColorState (0,3)

nextColorState :: MTGen -> ColorState -> IO (RGB.Colour, ColorState)
nextColorState g (ColorState (0,colorNum)) =
  do
    count <- P.round . (*240) <$> (random g :: IO Double)
    nextColorState g $ ColorState (count,(colorNum+1) `mod` 4)
nextColorState _ (ColorState (c,colorNum)) =
  return (color, ColorState (c-1, colorNum))
  where
    color =
      case colorNum of
        0 -> white
        1 -> oldGloryRed
        2 -> white
        3 -> oldGloryBlue
        _ -> error "Should not happen"

nextState :: MTGen -> SimState -> IO ((RGB.Colour, RGB.Colour, RGB.Colour), SimState)
nextState g (SimState eyeS noseS mouthS) =
  do
    (eyeColor, eyeS') <- nextColorState g eyeS
    (noseColor, noseS') <- nextColorState g noseS
    (mouthColor, mouthS') <- nextColorState g mouthS
    return ((eyeColor,noseColor,mouthColor), SimState eyeS' noseS' mouthS')

oldGloryRed :: RGB.Colour
oldGloryRed = RGB.RGB 0.698 0.132 0.203

oldGloryBlue :: RGB.Colour
oldGloryBlue = RGB.RGB 0.234 0.233 0.430

readMask :: FilePath -> IO (Field Bool)
readMask fp =
  do
    image <- readImage fp
    return $ run1 (A.map ((A.== 1) . (A.round :: Exp Float -> Exp Int) . luminance)) $ image
    
readImage :: FilePath -> IO (Field Colour)
readImage fp =
  do
    eimage <- readImageFromBMP fp
    case eimage of
      Left err -> P.print err >> exitFailure
      Right image -> return $ run1 (A.map unpackRGB) image

main :: IO ()
main = do
  [dir] <- getArgs
  pic <- readImage picLocation
  mask_pic <- readMask maskLocation
  nose_mask <- readMask noseMaskLocation
  mouth_mask <- readMask mouthMaskLocation
  eye_mask <- readMask eyeMaskLocation
  g <- newMTGen Nothing
  
  let
    dim@(Z :. height :. width) = A.arrayShape pic
    picSize = height*width
    empty_density = run $ A.fill (constant dim) (constant zero) :: Array DIM2 (Float,RGB.Colour)
    empty_velocity = run $ A.fill (constant dim) (constant (0,0)) :: Array DIM2 (Float,Float)
    
    idf = empty_density
    ivf = empty_velocity
    dt = 0.01
    dp = 0.0 
    dn = 0
    steps = 160
       
    preprocess
      :: Acc ( (Scalar (RGB.Colour,RGB.Colour,RGB.Colour), Field (Float,Float))
             , Field (Float,RGB.Colour)
             , Field (Float,Float)
             )
      -> (Acc (Field (Float,RGB.Colour)), Acc (Field (Float,Float)))
    preprocess tA =
      let
        (inputs,df,vf) = unlift3A tA
        (colors, noiseV) = unlift2A inputs
        (eyeColor,noseColor,mouthColor) = unlift3E $ the colors
        eyeColors = A.map (? (lift2E (2,eyeColor), constant zero)) $ use eye_mask
        noseColors = A.map (? (lift2E (2,noseColor), constant zero)) $ use nose_mask
        mouthColors = A.map (? (lift2E (2,mouthColor), constant zero)) $ use mouth_mask
        v = A.map (? (constant (0.05,-0.07), constant zero)) $ use mask_pic
        df' = A.zipWith3 (\e n m -> e .+. n .+. m) eyeColors noseColors mouthColors
        vf' = A.zipWith (.+.) v noiseV
      in
        (A.zipWith (.+.) df df', A.zipWith (.+.) vf vf')

    pipe = fluidPipe idf ivf $ colour pic . fluid steps dt dp dn . dampDensity 0.99 . preprocess
  
    producer :: Producer (Scalar (RGB.Colour, RGB.Colour, RGB.Colour), Field (Float,Float)) IO ()
    producer = f initialState
      where
        f simState =
          do
            (colors,simState') <- liftIO $ nextState g simState
            v' <- liftIO getVelocityNoise
            yield (toScalar colors, v')
            f simState'

    getVelocityNoise :: IO (Field (Float,Float))
    getVelocityNoise =
      do
        v1 <- S.replicateM picSize (fmap (toRange (-0.1,0.1)) $ random g)
        v2 <- S.replicateM picSize (fmap (toRange (-0.1,0.1)) $ random g)
        return $ fromVectors dim (((),v1),v2)

  runEffect $ producer >-> Pipes.take 7650 >-> pipe >-> writeToBMP dir

dampDensity
  :: FieldElt a
  => Exp Float
  -> (Acc (Field a), Acc VelocityField)
  -> (Acc (Field a), Acc VelocityField)
dampDensity damp (density,velocity) = (A.map (damp .*.) density, velocity)

colour
  :: Array DIM2 RGB.Colour
  -> (Acc (Field (Float, RGB.Colour)), Acc VelocityField)
  -> Acc (Field (Word32), Field (Float, RGB.Colour), VelocityField)
colour pic (df,vf) =
  let
    picA = use pic
  in lift3A (A.map packRGB $ A.zipWith joinPic picA df,df,vf)
  
joinPic :: Exp RGB.Colour -> Exp (Float,RGB.Colour) -> Exp RGB.Colour
joinPic c2 p1 =
  let
    (n,c1) = unlift p1
    nclamped = A.min n 1
  in
    blend nclamped (1-nclamped) c1 c2

toScalar :: Elt a => a -> Scalar a
toScalar = A.fromList Z . return

toRange :: (Float,Float) -> Double -> Float
toRange (minV,maxV) v =
  let f = realToFrac v
  in f * (maxV - minV) + minV

writeToBMP :: MonadIO m => FilePath -> Consumer (Array DIM2 Word32) m ()
writeToBMP fp =
  do
    liftIO $ createDirectory fp
    let
      go n = do
        arr <- await
        liftIO $ writeImageToBMP (fp P.++ "/" P.++ printf "%012d.bmp" n) arr
        go $ n+1
    go (0 :: Int)

    
  


