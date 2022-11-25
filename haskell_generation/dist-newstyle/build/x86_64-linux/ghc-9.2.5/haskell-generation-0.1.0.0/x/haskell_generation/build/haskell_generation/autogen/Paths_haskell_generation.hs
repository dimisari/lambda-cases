{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_generation (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/gnostis/.cabal/bin"
libdir     = "/home/gnostis/.cabal/lib/x86_64-linux-ghc-9.2.5/haskell-generation-0.1.0.0-inplace-haskell_generation"
dynlibdir  = "/home/gnostis/.cabal/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/gnostis/.cabal/share/x86_64-linux-ghc-9.2.5/haskell-generation-0.1.0.0"
libexecdir = "/home/gnostis/.cabal/libexec/x86_64-linux-ghc-9.2.5/haskell-generation-0.1.0.0"
sysconfdir = "/home/gnostis/.cabal/etc"

getBinDir     = catchIO (getEnv "haskell_generation_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_generation_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_generation_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_generation_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_generation_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_generation_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
