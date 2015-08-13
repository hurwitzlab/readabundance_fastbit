module Paths_Fbcsv (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/kyclark/work/readabundance_fastbit/haskell/.stack-work/install/x86_64-osx/lts-2.15/7.8.4/bin"
libdir     = "/Users/kyclark/work/readabundance_fastbit/haskell/.stack-work/install/x86_64-osx/lts-2.15/7.8.4/lib/x86_64-osx-ghc-7.8.4/Fbcsv-0.1.0.0"
datadir    = "/Users/kyclark/work/readabundance_fastbit/haskell/.stack-work/install/x86_64-osx/lts-2.15/7.8.4/share/x86_64-osx-ghc-7.8.4/Fbcsv-0.1.0.0"
libexecdir = "/Users/kyclark/.cabal/libexec"
sysconfdir = "/Users/kyclark/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Fbcsv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Fbcsv_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Fbcsv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Fbcsv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Fbcsv_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
