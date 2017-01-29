{-# LANGUAGE TypeFamilies #-}

module InputArray where

import Control.Concurrent

import Data.Array.Accelerate (Array, Z(..), (:.)(..), DIM2, Elt)
import Data.Array.Accelerate.Array.Sugar (EltRepr)
import Data.Array.Accelerate.IO (fromVectors,Vectors)

import Data.Vector.Storable as V
import Data.Vector.Storable.Mutable as MV

import Linear

import Prelude as P

data InputArray a = InputArray (V2 Int) (MVar (Vector a))
data InputArrayTuple a = InputArrayTuple (V2 Int) (MVar (Vector a, Vector a))

initializeInputArray
  :: (Num a, Storable a)
  => V2 Int
  -> IO (InputArray a)
initializeInputArray dim@(V2 x y) =
  do
    mvar <- newMVar $ V.replicate (x*y) 0
    return $ InputArray dim mvar
{-
getInput
  :: (Num a, Storable a, Elt a)
  => InputArray a
  -> IO (Array DIM2 a)
-}
getInput
  :: (Num t, Storable t, Elt e,
       Vectors (EltRepr e)
       ~ Vector t) =>
     InputArray t -> IO (Array DIM2 e)
getInput (InputArray (V2 x y) mvar) =
  do
    let v' = V.replicate (x*y) 0
    v <- swapMVar mvar v'
    return $ fromVectors (Z :. y :. x) v

addValues
  :: (Num a, Storable a)
  => InputArray a
  -> [(V2 Int, a)]
  -> IO ()
addValues (InputArray (V2 xdim ydim) mvar) list =
  do
    mv <- takeMVar mvar >>= unsafeThaw
    let vlen = MV.length mv
        f (V2 x y, v)
          | 0 <= x && x < xdim && 0 <= y && y < ydim = unsafeModify mv (+ v) i
          | otherwise = return ()
            where
              i = y * xdim + x
    P.mapM_ f list
    v <- unsafeFreeze mv
    putMVar mvar v


initializeInputArrayTuple
  :: (Num a, Storable a)
  => V2 Int
  -> IO (InputArrayTuple a)
initializeInputArrayTuple dim@(V2 x y) =
  do
    let v1 = V.replicate (x*y) 0
        v2 = V.replicate (x*y) 0
    mvar <- newMVar (v1,v2)
    return $ InputArrayTuple dim mvar
{-
getInput
  :: (Num a, Storable a, Elt a)
  => InputArray a
  -> IO (Array DIM2 a)
-}
{-}
getInputTuple
  :: (Num t, Storable t, Elt e,
       Vectors (EltRepr e)
       ~ Vector t) =>
     InputArrayTuple t -> IO (Array DIM2 e)
-}
--getInputTuple :: InputArrayTuple t -> IO (Array DIM2 (t,t))

getInputTuple
  :: (Num t, Storable t, Elt e, Vectors (EltRepr e) ~ (((), Vector t), Vector t))
  => InputArrayTuple t -> IO (Array DIM2 e)
getInputTuple (InputArrayTuple (V2 x y) mvar) =
  do
    let v1' = V.replicate (x*y) 0
    let v2' = V.replicate (x*y) 0
    (v1,v2) <- swapMVar mvar (v1',v2')
    return $ fromVectors (Z :. y :. x) (((),v1),v2)

addValuesTuple
  :: (Num a, Storable a)
  => InputArrayTuple a
  -> [(V2 Int, (a,a))]
  -> IO ()
addValuesTuple (InputArrayTuple (V2 xdim ydim) mvar) list =
  do
    (v1,v2) <- takeMVar mvar
    mv1 <- unsafeThaw v1
    mv2 <- unsafeThaw v2
    let vlen = xdim * ydim
        f (V2 x y, (a,b))
          | 0 <= x && x < xdim && 0 <= y && y < ydim = unsafeModify mv1 (+a) i >> unsafeModify mv2 (+b) i
          | otherwise = return ()
            where
              i = y * xdim + x
    P.mapM_ f list
    v1' <- unsafeFreeze mv1
    v2' <- unsafeFreeze mv2
    putMVar mvar (v1',v2')
