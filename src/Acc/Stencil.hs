{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Acc.Stencil where

import Data.Array.Accelerate as A

import Acc.Types

import Prelude as P

yxStencil3x3
  :: forall a. Elt a
  => (Stencil3x3 a -> Exp a)
  -> Acc (Cube a)
  -> Acc (Cube a)
yxStencil3x3 s = stencil f A.Clamp
  where
    f :: Stencil3x3x3 a -> Exp a
    f (_a,b,_c) = s b

yxStencil5x5
  :: forall a. Elt a
  => (Stencil5x5 a -> Exp a)
  -> Acc (Cube a)
  -> Acc (Cube a)
yxStencil5x5 s = stencil f A.Clamp
  where
    f :: Stencil5x5x3 a -> Exp a
    f (_a,b,_c) = s b

convolve5x5
  :: (P.Num (Exp a), Elt a, IsNum a)
  => [Exp a]
  -> Stencil5x5 a
  -> Exp a
convolve5x5 kernel
  (
    (a1,a2,a3,a4,a5),
    (b1,b2,b3,b4,b5),
    (c1,c2,c3,c4,c5),
    (d1,d2,d3,d4,d5),
    (e1,e2,e3,e4,e5)
  ) = P.sum $ P.zipWith (*) kernel
      [
        a1,a2,a3,a4,a5,
        b1,b2,b3,b4,b5,
        c1,c2,c3,c4,c5,
        d1,d2,d3,d4,d5,
        e1,e2,e3,e4,e5
      ]
