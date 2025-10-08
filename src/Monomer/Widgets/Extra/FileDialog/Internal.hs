{-# LANGUAGE PackageImports #-}

module Monomer.Widgets.Extra.FileDialog.Internal
  ( FileData(..)
  , PathKind(..)
  , getDirData
  , getDirData'
  , showSize
  ) where

import System.Directory.OsPath

import System.OsPath

import Control.Monad

import System.IO.Error qualified as IOE

import Data.Text qualified as T
import TextShow (showt, showb, toText)

import TextShow.Data.Floating

import Data.Time.Clock

data FileData = FileData
  { fdName :: OsPath
  , fdPath :: OsPath -- Full, absolutePath.
  , fdExtn :: Maybe OsString
  , fdKind :: PathKind
  , fdSize :: Maybe Integer
  , fdTime :: UTCTime
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
  dirs2 <- mapM (\fp -> FileData (takeFileName fp) fp Nothing           PathDir      Nothing         <$> getModificationTime fp) dirs
  fils2 <- mapM (\fp -> FileData (takeFileName fp) fp (getExtnMaybe fp) PathFile <$> getFileSize' fp <*> getModificationTime fp) fils
  return (dirs2, fils2)
  where
    getExtnMaybe :: OsPath -> Maybe OsString
    getExtnMaybe pt
      | (hasExtension pt) = Nothing
      | otherwise         = Just (takeExtensions pt)

getDirData' :: OsPath -> IO [FileData]
getDirData' pth = do
  (dirs, files) <- getDirData pth
  return (dirs ++ files)

getFileSize' :: OsPath -> IO (Maybe Integer)
getFileSize' osPath = Just <$> getFileSize osPath

showSize :: Integer -> T.Text
showSize n
  | n < 1024   = toText (showb n <> " B")
  | newM ==  1 = toText $ fltNum <> " KiB"
  | newM ==  2 = toText $ fltNum <> " MiB"
  | newM ==  3 = toText $ fltNum <> " GiB"
  | newM ==  4 = toText $ fltNum <> " TiB"
  | newM ==  5 = toText $ fltNum <> " PiB"
  | newM ==  6 = toText $ fltNum <> " EiB"
  | newM ==  7 = toText $ fltNum <> " ZiB"
  | newM ==  8 = toText $ fltNum <> " YiB"
  | newM ==  9 = toText $ fltNum <> " RiB"
  | newM == 10 = toText $ fltNum <> " QiB"
  | otherwise  = toText $ (showbEFloat (Just 2) nFlo) <> " B"
  where
    nFlo :: Double
    nFlo = fromInteger n
    mags = logBase 1024 nFlo
    newM = floor mags
    fltNum = showbFFloat (Just 2) (nFlo / (1024 ^^ newM))
