module Main where

import Control.Concurrent hiding (yield)
import Control.Monad (when,unless)

import Data.Array.Accelerate as A hiding ((>->),(||),(&&),(>=),(==),(<))
import Data.Array.Accelerate.Data.Colour.RGB as RGB
import Data.Array.Accelerate.IO (toVectors,fromVectors,readImageFromBMP)
import Data.Array.Accelerate.LLVM.PTX

import qualified Data.Vector.Storable as VS

import Fluid.Fluid
import Fluid.Pipe
import Fluid.Type
import Foreign.Ptr

import Graphics.Rendering.OpenGL as GL hiding (blend,get)
import qualified Graphics.UI.GLFW as G

import Pipes
import qualified Pipes.Prelude as Pipes
import Prelude as P

import System.Clock
import System.Exit

import Camera
import ColorCamera

keyCallback :: G.KeyCallback
keyCallback window key _scancode action _mods =
  when (key == G.Key'Escape && action == G.KeyState'Pressed) $
    G.setWindowShouldClose window True

readImage :: FilePath -> IO (Field Colour)
readImage fp =
  do
    eimage <- readImageFromBMP fp
    case eimage of
      Left err -> P.print err >> exitFailure
      Right image -> return $ run1 (A.map unpackRGB) image

joinPic :: Exp RGB.Colour -> Exp (Float,RGB.Colour) -> Exp RGB.Colour
joinPic c2 p1 =
  let
    (n,c1) = unlift p1
    nclamped = A.min n 1
  in
    blend nclamped (1-nclamped) c1 c2

process :: Array DIM2 Word32 -> VS.Vector Word32
process arr = toVectors arr

toData :: Ptr Word32 -> PixelData Word32
toData = PixelData BGRA UnsignedInt8888

height, width :: Int
height = 1080
width = 1920

step
  :: Array DIM2 Colour
  -> Acc ((Array DIM3 Word8, Array DIM3 Word8), Array DIM2 (Float,Colour), VelocityField)
  -> Acc (Array DIM2 Word32,Array DIM2 (Float,Colour),VelocityField)
step sourcePic acc =
  let
    dt = 0.01
    dp = 0.0
    dn = 0
    steps = 160
    (df,vf) = fromDoublePicVelocityOnly acc
    sourcePicA = use $ run1 (A.map (\x -> A.lift (0.2 :: Exp Float,x))) sourcePic
    df' = A.map (0.99 .*.) $ A.zipWith (.+.) sourcePicA df
    (df'',vf'') = fluid steps dt dp dn (df',vf')
    pic = A.map A.snd df''
  in
    A.lift (A.map packRGB pic,df'',vf'')

main :: IO ()
main =
  do
    let
      s = Size (P.fromIntegral width) (P.fromIntegral height)
    _b <- G.init
    mw@(Just window) <- G.createWindow width height "mectron-colors" Nothing Nothing
    image <- readImage "/home/mectron/Desktop/MECTRON.bmp"
    prevTimeMVar <- getTime Monotonic >>= newMVar
    producer <-
      do
        mcamera <- cameraProducer width height
        case mcamera of
          Nothing ->
            do
              putStrLn "Failed to find a camera, closing..."
              exitFailure
          Just (_,camera) ->
            do
              let dim = Z :. height :. width :. 3
              return (camera >-> Pipes.map (fromVectors dim)) :: IO (Producer (Array DIM3 Word8) IO ())
    let
      idf = run $ A.fill (A.lift $ Z :. height :. width) $ A.lift $ constant zero :: Array DIM2 (Float,Colour)
      idv = run $ A.fill (A.lift $ Z :. height :. width) $ A.lift $ constant zero :: Array DIM2 (Float,Float)
      pipe = fluidPipe idf idv (step image)
      consumer :: Consumer (Array DIM2 Word32) IO ()
      consumer = consumer' (0 :: Int)
        where
          consumer' count =
            do
              arr <- await
              let v = process arr
              close <- liftIO $ do
                VS.unsafeWith v (drawPixels s . toData)
                G.pollEvents
                G.swapBuffers window
                when (count `mod` 100 == 0) $
                  do
                    prevTime <- takeMVar prevTimeMVar
                    currTime <- getTime Monotonic
                    putMVar prevTimeMVar currTime
                    let nanos = P.fromIntegral $ toNanoSecs $ diffTimeSpec prevTime currTime :: Double
                    print $ 100*1e9 / nanos
                G.windowShouldClose window
              unless close $ consumer' (count+1)
    G.makeContextCurrent mw
    G.setCursorInputMode window $ G.CursorInputMode'Hidden
    G.setKeyCallback window $ Just keyCallback
    GL.clearColor $= Color4 0 0 0 0
    GL.shadeModel $= Flat
    GL.rowAlignment Unpack $= 1
    G.swapBuffers window
--    runEffect $ producer >-> consumer
    G.destroyWindow window
    G.terminate
    exitSuccess
