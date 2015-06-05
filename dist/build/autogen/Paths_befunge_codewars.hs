module Paths_befunge_codewars (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/catherine/.cabal/bin"
libdir     = "/home/catherine/.cabal/lib/x86_64-linux-ghc-7.8.3/befunge-codewars-0.1.0.0"
datadir    = "/home/catherine/.cabal/share/x86_64-linux-ghc-7.8.3/befunge-codewars-0.1.0.0"
libexecdir = "/home/catherine/.cabal/libexec"
sysconfdir = "/home/catherine/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "befunge_codewars_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "befunge_codewars_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "befunge_codewars_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "befunge_codewars_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "befunge_codewars_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
