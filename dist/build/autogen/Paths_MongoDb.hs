module Paths_MongoDb (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/rick/.cabal/bin"
libdir     = "/home/rick/.cabal/lib/MongoDb-0.0.1/ghc-6.10.4"
datadir    = "/home/rick/.cabal/share/MongoDb-0.0.1"
libexecdir = "/home/rick/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "MongoDb_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "MongoDb_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "MongoDb_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "MongoDb_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
