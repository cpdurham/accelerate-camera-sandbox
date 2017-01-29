{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE TemplateHaskell  #-}

module Fluid.Config where

import Fluid.Type

import Data.Label
import Control.Monad
import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A
import Data.Array.Accelerate.LLVM.PTX

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

data Initial a
    = FromFile          FilePath
    | FromFunction      (Int -> Int -> a)

data Source = Camera | Mouse | Leap
  deriving (Show,Read)

data Config = Config
  {
    -- simulation
    _viscosity          :: !Float
  , _diffusion          :: !Float
  , _timestep           :: !Float
--  , _inputDensity       :: !Float
--  , _inputVelocity      :: !Float
  , _simulationSteps    :: !Int
  , _simulationWidth    :: !Int
  , _simulationHeight   :: !Int

  -- visualisation
--  , _displayScale       :: !Int
--  , _displayFramerate   :: !Int
  , _initialDensity     :: DensityField
  , _initialVelocity    :: VelocityField
  , _initialPicture     :: Pic
  , _inputSource        :: !Source

  -- extra options to specify initial conditions for command parsing
  , _setupDensity       :: Initial DensityField
  , _setupVelocity      :: Initial VelocityField
  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _viscosity          = 0
  , _diffusion          = 0
  , _timestep           = 0.1
--  , _inputDensity       = 50
--  , _inputVelocity      = 20
  , _simulationSteps    = 40
  , _simulationWidth    = 100
  , _simulationHeight   = 100

  , _initialDensity     = error "initial density??"
  , _initialVelocity    = error "initial velocity??"
  , _initialPicture     = error "initial picture??"
  , _inputSource          = Mouse

  , _setupDensity       = FromFunction makeField_empty
  , _setupVelocity      = FromFunction makeField_empty
  }

getUsage :: IO String
getUsage =
  do
    prg <- getProgName
    return $ usageInfo prg options

options :: [OptDescr (Config -> IO Config)]
options =
  [ Option  [] ["viscosity"]
            (ReqArg (\arg opt -> return $ parse viscosity arg opt) "FLOAT")
            (describe viscosity "viscosity for velocity damping")

  , Option  [] ["diffusion"]
            (ReqArg (\arg opt -> return $ parse diffusion arg opt) "FLOAT")
            (describe diffusion "diffusion rate for mass dispersion")

  , Option  [] ["delta"]
            (ReqArg (\arg opt -> return $ parse timestep arg opt) "FLOAT")
            (describe timestep "simulation time between each frame")
{-
  , Option  [] ["density"]
            (ReqArg (parse inputDensity) "FLOAT")
            (describe inputDensity "magnitude of user input density")

  , Option  [] ["velocity"]
            (ReqArg (parse inputVelocity) "FLOAT")
            (describe inputVelocity "magnitude of user input velocity")
-}

  , Option  [] ["width"]
            (ReqArg (\arg opt -> return $ parse simulationWidth arg opt) "INT")
            (describe simulationWidth "grid width of simulation")

  , Option  [] ["height"]
            (ReqArg (\arg opt -> return $ parse simulationHeight arg opt) "INT")
            (describe simulationHeight "grid height of simulation")

  , Option  [] ["source"]
            (ReqArg (\arg opt -> return $ parse inputSource arg opt) "Camera|Mouse|Leap")
            (describe inputSource "input source to fluid")

  -- Initial conditions
  , Option  [] ["init-checks"]
            (NoArg (return . init_checks))
            "initial density field with zero velocity field"

  , Option  [] ["init-man"]
            (NoArg (return . init_man))
            "initial density field with swirling velocity"

  , Option  [] ["init-elk"]
            (NoArg (return . init_elk))
            "initial density field with swirling velocity"
    
  , Option "h" ["help"]
    (NoArg
     (\_ -> do
         usage <- getUsage
         hPutStrLn stderr usage
         exitWith ExitSuccess))
    "Show help"
  ]
  where
    parse f x           = set f (read x)
    describe f msg      = msg P.++ " (" P.++ show (get f defaults) P.++ ")"

    init_checks         = set setupDensity  (FromFunction makeDensity_checks)
                        . set setupVelocity (FromFunction makeField_empty)

    init_man            = set setupDensity  (FromFunction makeDensity_checks)
                        . set setupVelocity (FromFunction makeVelocity_man)

    init_elk            = set setupDensity  (FromFunction makeDensity_checks)
                        . set setupVelocity (FromFunction makeVelocity_elk)


parseArgs :: IO Config
parseArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaults) actions
    initialiseConfig opts

initialiseConfig :: Config -> IO Config
initialiseConfig conf = do
  let
    width     = get simulationWidth conf
    height    = get simulationHeight conf

  dens  <- case get setupDensity conf of
--              FromFile fn       -> loadDensity_bmp fn width height
              FromFunction f    -> return (f width height)

  velo  <- case get setupVelocity conf of
--              FromFile fn       -> loadVelocity_bmp fn width height
              FromFunction f    -> return (f width height)

  let
    pic = A.fromList (Z :. height :. width) $ repeat (0,0,0)
    conf'     = set initialDensity  dens
                . set initialVelocity velo
                . set initialPicture pic
                $ conf

  return conf'


makeField_empty :: FieldElt e => Int -> Int -> Field e
makeField_empty width height
  = run $ A.fill (constant (Z:.height:.width)) (constant zero)


makeDensity_checks :: Int -> Int -> DensityField
makeDensity_checks width height
  = let width'  = constant $ P.fromIntegral width
        height' = constant $ P.fromIntegral height
        yc      = constant $ P.fromIntegral (height `div` 2)
        xc      = constant $ P.fromIntegral (width  `div` 2)

        checks ix
          = let Z :. y :. x     = unlift ix
                x'              = A.fromIntegral x
                y'              = A.fromIntegral y
                tx              = 10 * (x' - xc) / width'
                ty              = 10 * (y' - yc) / height'
                xk1             = abs tx A.> 3*pi/2 ? (0 , cos tx)
                yk1             = abs ty A.> 3*pi/2 ? (0 , cos ty)
                d1              = xk1 * yk1
            in
            0 `A.max` d1
    in
    run $ A.generate (constant (Z:.height:.width)) checks


makeVelocity_man :: Int -> Int -> VelocityField
makeVelocity_man width height
  = let width'  = constant $ P.fromIntegral width
        height' = constant $ P.fromIntegral height
        yc      = constant $ P.fromIntegral (height `div` 2)
        xc      = constant $ P.fromIntegral (width  `div` 2)

        man ix
          = let Z :. y :. x     = unlift ix
                x'              = A.fromIntegral x
                y'              = A.fromIntegral y
                xk2             = cos (19 * (x' - xc) / width')
                yk2             = cos (17 * (y' - yc) / height')
                d2              = xk2 * yk2 / 5
            in
            lift (constant 0, d2)
    in
    run $ A.generate (constant (Z:.height:.width)) man


makeVelocity_elk ::  Int -> Int -> VelocityField
makeVelocity_elk width height
  = let width'  = constant $ P.fromIntegral width
        height' = constant $ P.fromIntegral height
        yc      = constant $ P.fromIntegral (height `div` 2)
        xc      = constant $ P.fromIntegral (width  `div` 2)

        elk ix
          = let Z :. y :. x     = unlift ix
                x'              = A.fromIntegral x
                y'              = A.fromIntegral y
                tx              = 12 * (x' - xc) / width'
                ty              = 12 * (y' - yc) / height'
                xk2             =  cos tx
                yk2             = -cos ty
                d2              = xk2 * yk2 / 5
            in
            lift (constant 0, d2)
    in
    run $ A.generate (constant (Z:.height:.width)) elk

