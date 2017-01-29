module Acc.Gaussian where

import Data.Array.Accelerate

import Prelude as P

import Acc.Stencil

gaussianSigStencil :: Float -> Stencil5x5 Float -> Exp Float
gaussianSigStencil sigma =
    let
      f (u,v) =
        let
          uc = u - (5-1)/2
          vc = v - (5-1)/2
        in
          exp (-(uc*uc+vc*vc)/(2*sigma*sigma))
      vs = P.map f [(x,y) | x <- [0..4], y <- [0..4]]
      vsSum = P.sum vs
      gaussianF = P.map constant $ P.map (/vsSum) vs
    in
      convolve5x5 gaussianF
