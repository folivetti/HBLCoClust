{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_HBLCoClust (
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

bindir     = "bin"
libdir     = "/home/olivetti/.cabal/lib/x86_64-linux-ghc-8.0.2/HBLCoClust-0.1.0.0-A6FkcHGnC2WBfu88ZyBFTo"
dynlibdir  = "/home/olivetti/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/olivetti/.cabal/share/x86_64-linux-ghc-8.0.2/HBLCoClust-0.1.0.0"
libexecdir = "/home/olivetti/.cabal/libexec"
sysconfdir = "/home/olivetti/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HBLCoClust_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HBLCoClust_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HBLCoClust_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HBLCoClust_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HBLCoClust_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HBLCoClust_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
