{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Acc.Lift where

import Data.Array.Accelerate

lift2A
  :: (Arrays a, Arrays b)
  => (Acc a, Acc b)
  -> Acc (a,b)
lift2A = lift

lift3A
  :: (Arrays a, Arrays b, Arrays c)
  => (Acc a, Acc b, Acc c)
  -> Acc (a,b,c)
lift3A = lift

lift2E
  :: (Elt a, Elt b)
  => (Exp a, Exp b)
  -> Exp (a,b)
lift2E = lift

lift3E
  :: (Elt a, Elt b, Elt c)
  => (Exp a, Exp b, Exp c)
  -> Exp (a,b,c)
lift3E = lift

lift4E
  :: (Elt a, Elt b, Elt c, Elt d)
  => (Exp a, Exp b, Exp c, Exp d)
  -> Exp (a,b,c,d)
lift4E = lift

lift6E
  :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
  => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
  -> Exp (a,b,c,d,e,f)
lift6E = lift

unlift2A
  :: (Arrays a, Arrays b)
     => Acc (a,b)
     -> (Acc a, Acc b)
unlift2A = unlift

unlift3A
  :: (Arrays a, Arrays b, Arrays c)
     => Acc (a,b,c)
     -> (Acc a, Acc b, Acc c)
unlift3A = unlift

unlift4A
  :: (Arrays a, Arrays b, Arrays c, Arrays d)
     => Acc (a,b,c,d)
     -> (Acc a, Acc b, Acc c, Acc d)
unlift4A = unlift

unlift5A
  :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e)
     => Acc (a,b,c,d,e)
     -> (Acc a, Acc b, Acc c, Acc d, Acc e)
unlift5A = unlift

unlift6A
  :: (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f)
     => Acc (a,b,c,d,e,f)
     -> (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f)
unlift6A = unlift

unlift2E
  :: (Elt a, Elt b)
     => Exp (a,b)
     -> (Exp a, Exp b)
unlift2E = unlift

unlift3E
  :: (Elt a, Elt b, Elt c)
     => Exp (a,b,c)
     -> (Exp a, Exp b, Exp c)
unlift3E = unlift

unlift4E
  :: (Elt a, Elt b, Elt c, Elt d)
     => Exp (a,b,c,d)
     -> (Exp a, Exp b, Exp c, Exp d)
unlift4E = unlift

unlift5E
  :: (Elt a, Elt b, Elt c, Elt d, Elt e)
     => Exp (a,b,c,d,e)
     -> (Exp a, Exp b, Exp c, Exp d, Exp e)
unlift5E = unlift

unlift6E
  :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
     => Exp (a,b,c,d,e,f)
     -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
unlift6E = unlift

unlift7E
  :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
     => Exp (a,b,c,d,e,f,g)
     -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
unlift7E = unlift

unlift8E
  :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
     => Exp (a,b,c,d,e,f,g,h)
     -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
unlift8E = unlift

index3
  :: (Elt i, Slice (Z :. i), Slice (Z :. i :. i))
  => Exp i
  -> Exp i
  -> Exp i
  -> Exp (Z :. i :. i :. i)
index3 i j k = lift (Z :. i :. j :. k)

unindex3
  :: forall i. (Elt i, Slice (Z :. i), Slice (Z :. i :. i))
  => Exp (Z :. i :. i :. i)
  -> Exp (i, i, i)
unindex3 ix =
  let
    Z :. i :. j :. k = unlift ix :: Z :. Exp i :. Exp i :. Exp i
  in lift (i, j, k)

index4
  :: (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
  => Exp i
  -> Exp i
  -> Exp i
  -> Exp i
  -> Exp (Z :. i :. i :. i :. i)
index4 i j k w = lift (Z :. i :. j :. k :. w)

unindex4
  :: forall i.
     (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
  => Exp (Z :. i :. i :. i :. i)
  -> Exp (i, i, i, i)
unindex4 ix =
  let
    Z :. i :. j :. k :. w = unlift ix :: Z :. Exp i :. Exp i :. Exp i :. Exp i
  in lift (i, j, k, w)

