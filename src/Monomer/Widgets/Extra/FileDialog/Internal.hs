module Monomer.Widgets.Extra.FileDialog.Internal
  ( FileData(..)
  , PathKind(..)
  , getDirData
  ) where

import System.Directory.OsPath

import System.OsPath

import Control.Monad

import System.IO.Error qualified as IOE

data FileData = FileData
  { fdName :: OsPath
  , fdPath :: OsPath -- Full, absolutePath.
  , fdExtn :: Maybe OsString
  , fdKind :: PathKind
  } deriving (Show, Eq)

data PathKind
  = PathFile
  | PathDir
  deriving (Show, Eq)

-- | Get all directories and files in a given 
--   directory. 
getDirData :: OsPath -> IO ([FileData],[FileData])
getDirData pth = do
  pths <- map (pth </>) <$> listDirectory pth
  dirs <- filterM doesDirectoryExist pths
  fils <- filterM doesFileExist pths
  let dirs2 = map (\fp -> FileData (takeFileName fp) fp Nothing           PathDir ) dirs
      fils2 = map (\fp -> FileData (takeFileName fp) fp (getExtnMaybe fp) PathFile) fils
  return (dirs2, fils2)
  where
    getExtnMaybe :: OsPath -> Maybe OsString
    getExtnMaybe pt
      | (hasExtension pt) = Nothing
      | otherwise         = Just (takeExtensions pt)










