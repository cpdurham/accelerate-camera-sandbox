module Pipes.Disk where

import Control.Monad (forever)

import qualified Data.Text as T
import Data.Text.Encoding

import qualified Data.Vector.Storable as S

import Data.Word

import Foreign.ForeignPtr

import Text.Printf

import Pipes

import System.IO

type ImageDim = (Int,Int,Int)

vectorWriter
  :: FilePath
  -> ImageDim
  -> Consumer (S.Vector Word8) IO ()
vectorWriter fp dim =
  do
    createDirectory fp
    writeFile (fp ++ "/size") $ show dim
    let
      go n = do
        v <- await
        let path = fp ++ "/" ++ printf "%012d.vec" n 
        liftIO $ vectorWrite path v
        go $ n+1
    go 0

vectorWrite :: FilePath -> S.Vector Word8 -> IO ()
vectorWrite fp v =
  do
    let (fptr,size) = S.unsafeToForeignPtr0 v
    withBinaryFile fp WriteMode (\h -> withForeignPtr fptr (\ptr -> hPutBuf h ptr size))

vectorReader
  :: Filepath
  -> (ImageDim, Producer (S.Vector Word8) IO ())
vectorReader fp =
  do
    (h,w,d) <- read <$> readFile (fp ++ "/" ++ "size")
    imageSize = h*w*d
    files <-
      P.map (\x -> fp ++ "/" ++ x) .
      sort . P.filter (".vec" `isSuffixOf`) <$>
      listDirectory fp
    return (dim,mapM_ (\f -> liftIO (vectorRead f imageSize) >>= yield) files)

vectorRead :: FilePath -> Int -> IO (S.Vector Word8)
vectorRead fp size =
  do
    let
      v = S.replicate size 0
      (fptr,s) = S.unsafeToForeignPtr0 v
    _ <- withBinaryFile fp ReadMode (\h -> withForeignPtr fptr (\ptr -> hGetBuf h ptr s))
    return v

