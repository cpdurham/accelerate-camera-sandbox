module Main where

import Camera
import ColorCamera

import Config
import Fluid.Fluid (fluid)
import Fluid.Pipe
import Fluid.Type
import Data.Label
import qualified Data.Vector.Storable as VS
import Control.Concurrent hiding (yield)
import Control.Monad (forever,when, unless)

import Foreign.Ptr
import System.Clock

import GHC.Float

import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.UI.GLFW as G

import Pipes
import Prelude                                          as P
import Data.Array.Accelerate                            as A hiding ((>->),(||),(&&),(>=),(==),(<))
import Data.Array.Accelerate.IO (toVectors)

import System.Exit
import System.IO


import Linear
import Data.Default
import Data.Maybe (catMaybes)

import InputArray

errorCallback :: G.ErrorCallback
errorCallback _err description = hPutStrLn stderr description

keyCallback :: G.KeyCallback
keyCallback window key _scancode action _mods =
  when (key == G.Key'Escape && action == G.KeyState'Pressed) $
    G.setWindowShouldClose window True

mouseCallback :: Int -> MVar (Float,Float) -> InputArrayTuple Float -> G.CursorPosCallback
mouseCallback height prevPosMVar inputArray _window x1' y1' =
  do
    let
      x1 = double2Float x1'
      y1 = double2Float y1'
      y = height - 1 - P.truncate y1
      x = P.truncate x1
    (y0,x0) <- swapMVar prevPosMVar (y1,x1)
    let
      e = (V2 x y, (y1 - y0,x1 - x0))
    addValuesTuple inputArray [e]

filterInRange :: DIM2 -> [(V2 Int,a)] -> [(DIM2,a)]
filterInRange (Z :. ydim :. xdim) = catMaybes . P.map f
  where f (V2 x y,a)
          | x < 0 || x >= xdim || y < 0 || y >= ydim = Nothing
          | otherwise = Just $ (Z :. y :. x,a)

sourcesFromList :: Elt a => [(DIM2,a)] -> (Vector DIM2, Vector a)
sourcesFromList s =
  let (ix, ss)  = P.unzip s
      sh        = Z :. P.length ix
  in  ( A.fromList sh ix, A.fromList sh ss )

main :: IO ()
main = do
  conf <- parseArgs
  let
    width     = get windowWidth  conf
    height    = get windowHeight conf
    simtype   = get simType conf
    source    = get inputSource conf
    colorationMethod = get coloration conf
    
    idf       = get initialDensity conf
    ivf       = get initialVelocity conf

    steps     = get simulationSteps  conf
    dp        = get viscosity conf
    dn        = get diffusion conf
    dt        = get timestep conf

    dim@(Z :. ydim :. xdim) = arrayShape idf
    vdim = V2 xdim ydim
    s = Size (P.fromIntegral width) (P.fromIntegral height)

  _b <- G.init
  mw@(Just window) <- G.createWindow width height "accelerate-camera-sandbox" Nothing Nothing

  prevLocMVar <- newMVar (0,0) :: IO (MVar (Float,Float))
  prevTimeMVar <- getTime Monotonic >>= newMVar
  let --for fluid only
    colorationPipe =
      case colorationMethod of
        Colorful -> colorAngles
        Halloween -> colorAnglesHalloween
        BlackWhite -> simple
  producer <-
    case source of
      Mouse ->
        do
          inputArrayTuple <- initializeInputArrayTuple vdim
          G.setCursorPosCallback window $ Just (mouseCallback height prevLocMVar inputArrayTuple)
          let
            ds = A.fromList dim $ repeat 0.1 :: DensityField
            producer =
              forever $
              do
                vs <- liftIO $ getInputTuple inputArrayTuple
                yield (ds,vs)
            pipe = fluidPipe idf ivf $ colorationPipe . fluid steps dt dp dn . fromArraySource
          return $ producer >-> pipe
      Camera ->
        do
          camera <-
            do
              mcamera <- cameraArrayProducer width height
              case mcamera of
                Nothing ->
                  do
                    putStrLn "Failed to find a camera, closing..."
                    exitFailure
                Just camera -> return camera
          case simtype of
            Fluid ->
              let pipe = fluidPipe idf ivf $ colorationPipe . fluid steps dt dp dn . fromDoublePic
              in return $ camera >-> doublePic >-> pipe
            GradientLuminance -> return $ camera >-> gradientLuminancePipe
            Gradient -> return $ camera >-> gradientPipe
            Simple -> return $ camera >-> simplePipe
  let
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
  runEffect $ producer >-> consumer
  G.destroyWindow window
  G.terminate
  exitSuccess

process :: Array DIM2 (Word8,Word8,Word8) -> VS.Vector (Color3 GLubyte)
process arr =
  let ((((),bs),gs),rs) = toVectors arr
  in VS.zipWith3 toColor bs gs rs

toData :: Ptr (Color3 GLubyte) -> PixelData (Color3 GLubyte)
toData = PixelData BGR UnsignedByte

toColor :: Word8 -> Word8 -> Word8 -> Color3 GLubyte
toColor r g b =
  let
    r' = P.fromIntegral r
    g' = P.fromIntegral g
    b' = P.fromIntegral b
  in Color3 b' g' r'
