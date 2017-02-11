{-# Language ScopedTypeVariables #-}

module Pipes.Formats where

import Control.Monad as M

import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO as A

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM

import Pipes
import Prelude as P

vectorToArray :: (Int,Int,Int) -> Pipe (S.Vector Word8) (Array DIM3 Word8) IO ()
vectorToArray (h,w,d) =
  let
    dim = Z :. h :. w :. d
  in
    forever $ do
      v <- await
      yield $ fromVectors dim v

-- | Takes in vectors and 'chunks' them out as int x dim size vectors
--
vectorsToChunks
  :: forall m. MonadIO m
  => Int -> (Int,Int,Int) -> Pipe (S.Vector Word8) (S.Vector Word8) m ()
vectorsToChunks limit (h,w,d) =
  let
    maxCubeSize = limit * imageSize
    imageSize = (h*w*d)
    cubeAggregator
      :: Pipe (S.Vector Word8) (S.Vector Word8) m ()
    cubeAggregator =
      do
        mem <- liftIO $ SM.new maxCubeSize
        M.forM_ (P.init [0..limit]) $
          (\i ->
              do
                arr <- await
                let
                  sliced = SM.unsafeSlice (i*imageSize) (imageSize) mem
                thawed <- liftIO $ S.unsafeThaw arr
                liftIO $ SM.copy sliced thawed
          )
        liftIO (S.unsafeFreeze mem) >>= yield
  in
    forever $ cubeAggregator

-- | Takes in chunks and slices them out as int number of dim vectors
--
chunksToVectors
  :: MonadIO m
  => Int -> (Int,Int,Int) -> Pipe (S.Vector Word8) (S.Vector Word8) m ()
chunksToVectors limit (h,w,d) =
  let
    imageSize = h*w*d
    slicer =
      do
        chunk <- await
        mapM_ yield $ P.map (\i -> S.slice (i*imageSize) imageSize chunk) $ P.init [0..limit]
  in
    forever slicer
 
