{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ImageProcessing (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nakamura/OneDrive/haskell/ImageProcessing/.stack-work/install/x86_64-linux-tinfo6/65c8235611c40fdf577417f70c3fa81d8a60becf9727917b0cf3459400938d1c/8.10.7/bin"
libdir     = "/home/nakamura/OneDrive/haskell/ImageProcessing/.stack-work/install/x86_64-linux-tinfo6/65c8235611c40fdf577417f70c3fa81d8a60becf9727917b0cf3459400938d1c/8.10.7/lib/x86_64-linux-ghc-8.10.7/ImageProcessing-0.1.0.0-1P7bQrWlxW6Bc7ko6kDkCb-ImageProcessing-exe"
dynlibdir  = "/home/nakamura/OneDrive/haskell/ImageProcessing/.stack-work/install/x86_64-linux-tinfo6/65c8235611c40fdf577417f70c3fa81d8a60becf9727917b0cf3459400938d1c/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/nakamura/OneDrive/haskell/ImageProcessing/.stack-work/install/x86_64-linux-tinfo6/65c8235611c40fdf577417f70c3fa81d8a60becf9727917b0cf3459400938d1c/8.10.7/share/x86_64-linux-ghc-8.10.7/ImageProcessing-0.1.0.0"
libexecdir = "/home/nakamura/OneDrive/haskell/ImageProcessing/.stack-work/install/x86_64-linux-tinfo6/65c8235611c40fdf577417f70c3fa81d8a60becf9727917b0cf3459400938d1c/8.10.7/libexec/x86_64-linux-ghc-8.10.7/ImageProcessing-0.1.0.0"
sysconfdir = "/home/nakamura/OneDrive/haskell/ImageProcessing/.stack-work/install/x86_64-linux-tinfo6/65c8235611c40fdf577417f70c3fa81d8a60becf9727917b0cf3459400938d1c/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ImageProcessing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ImageProcessing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ImageProcessing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ImageProcessing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ImageProcessing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ImageProcessing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
