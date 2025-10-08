module Monomer.Widgets.Extra.FileDialog.Internal
  (

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

getDirData :: OsPath -> IO ([FileData],[FileData])
getDirData pth = do
  pths <- map (pth </>) <$> listDirectory
  dirs <- filterM doesDirectoryExist pths
  fils <- filterM doesFileExist pths
  let dirs2 = map (\fp -> FileData (takeFileName fp) fp Nothing           PathDir ) dirs
      fils2 = map (\fp -> FileData (takeFileName fp) fp (getExtnMaybe fp) PathFile) fils
  return (fils2, dirs2)
  where
    getExtnMaybe :: OsPath -> Maybe OsString
    getExtnMaybe pt
      | (hasExtension pt) = Nothing
      | otherwise         = Just (takeExtensions pt)










