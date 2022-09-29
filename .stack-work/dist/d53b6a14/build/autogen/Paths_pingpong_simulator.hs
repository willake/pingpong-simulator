{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_pingpong_simulator (
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
version = Version [0,3,1,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\uu\\Motion and Manipulation\\simulators\\pingpong-simulator-0312-experimental\\.stack-work\\install\\1aa4ad29\\bin"
libdir     = "E:\\uu\\Motion and Manipulation\\simulators\\pingpong-simulator-0312-experimental\\.stack-work\\install\\1aa4ad29\\lib\\x86_64-windows-ghc-9.0.2\\pingpong-simulator-0.3.1.2-KtnDO6ySYrX1qs3kHbo9zk"
dynlibdir  = "E:\\uu\\Motion and Manipulation\\simulators\\pingpong-simulator-0312-experimental\\.stack-work\\install\\1aa4ad29\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "E:\\uu\\Motion and Manipulation\\simulators\\pingpong-simulator-0312-experimental\\.stack-work\\install\\1aa4ad29\\share\\x86_64-windows-ghc-9.0.2\\pingpong-simulator-0.3.1.2"
libexecdir = "E:\\uu\\Motion and Manipulation\\simulators\\pingpong-simulator-0312-experimental\\.stack-work\\install\\1aa4ad29\\libexec\\x86_64-windows-ghc-9.0.2\\pingpong-simulator-0.3.1.2"
sysconfdir = "E:\\uu\\Motion and Manipulation\\simulators\\pingpong-simulator-0312-experimental\\.stack-work\\install\\1aa4ad29\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pingpong_simulator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pingpong_simulator_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pingpong_simulator_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pingpong_simulator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pingpong_simulator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pingpong_simulator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
