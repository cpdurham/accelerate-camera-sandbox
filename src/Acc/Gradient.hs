module Acc.Gradient where

import Data.Array.Accelerate as A

import Acc.Types

gradientX :: Acc (Image Float) -> Acc (Image Float)
gradientX = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((u, _, x),
          (v, _, y),
          (w, _, z)
         ) = x + (2*y) + z - u - (2*v) - w


gradientY :: Acc (Image Float) -> Acc (Image Float)
gradientY = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((x, y, z),
          (_, _, _),
          (u, v, w)
         ) = x + (2*y) + z - u - (2*v) - w

gradientMag
  :: Acc (Image Float)
  -> Acc (Array DIM2 Float)
gradientMag = stencil magdir Clamp
  where
    magdir :: Stencil3x3 Float -> Exp Float
    magdir ((v0, v1, v2)
           ,(v3,  _, v4)
           ,(v5, v6, v7)) =
      let
        dx = v2 + (2*v4) + v7 - v0 - (2*v3) - v5
        dy = v0 + (2*v1) + v2 - v5 - (2*v6) - v7
        mag = sqrt (dx * dx + dy * dy)
      in mag

gradientMagStencil :: Stencil3x3 Float -> Exp Float
gradientMagStencil
  ( (v0, v1, v2)
  , (v3,  _, v4)
  , (v5, v6, v7)
  ) =
    let
      dx = v2 + (2*v4) + v7 - v0 - (2*v3) - v5
      dy = v0 + (2*v1) + v2 - v5 - (2*v6) - v7
    in sqrt (dx * dx + dy * dy)
{-
gradientMagDir
  :: Acc (Image Float)
  -> Acc (Array DIM2 (Float,Int))
gradientMagDir = stencil magdir Clamp
  where
    magdir :: Stencil3x3 Float -> Exp (Float,Int)
    magdir ((v0, v1, v2)
           ,(v3,  _, v4)
           ,(v5, v6, v7)) =
      let
          -- Image gradients
          dx          = v2 + (2*v4) + v7 - v0 - (2*v3) - v5
          dy          = v0 + (2*v1) + v2 - v5 - (2*v6) - v7

          -- Magnitude
          mag         = sqrt (dx * dx + dy * dy)

          -- Direction
          --
          -- Determine the angle of the vector and rotate it around a bit to
          -- make the segments easier to classify
          theta       = atan2 dy dx
          alpha       = (theta - (pi/8)) * (4/pi)

          -- Normalise the angle to between [0..8)
          norm        = alpha + 8 * A.fromIntegral (boolToInt (alpha <=* 0))

          -- Try to avoid doing explicit tests, to avoid warp divergence
          dir = A.bit $ A.floor norm
        --  dir         = boolToInt (A.not undef) * ((64 * (1 + A.floor norm `mod` 4)) `min` 255)
      in
       lift (mag, dir)
-}

gradientMagDirFree
  :: Acc (Image Float)
  -> Acc (Array DIM2 (Float,Float))
gradientMagDirFree = stencil magdir Clamp
  where
    magdir :: Stencil3x3 Float -> Exp (Float,Float)
    magdir ((v0, v1, v2)
           ,(v3,  _, v4)
           ,(v5, v6, v7)) =
      let
          -- Image gradients
          dx          = v2 + (2*v4) + v7 - v0 - (2*v3) - v5
          dy          = v0 + (2*v1) + v2 - v5 - (2*v6) - v7

          -- Magnitude
          mag         = sqrt (dx * dx + dy * dy)

          -- Direction
          --
          -- Determine the angle of the vector and rotate it around a bit to
          -- make the segments easier to classify
          theta       = A.atan2 dy dx
          alpha       = (theta - (pi/8)) * (4/pi)

          -- Normalise the angle to between [0..8)
          norm        = alpha + 8 * A.fromIntegral (boolToInt (alpha A.<= 0))

          -- Try to avoid doing explicit tests, to avoid warp divergence
          dir = norm
        --  dir         = boolToInt (A.not undef) * ((64 * (1 + A.floor norm `mod` 4)) `min` 255)
      in
       lift (mag, dir)
