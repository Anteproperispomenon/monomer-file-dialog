{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module Monomer.Widgets.Extra.FileDialog.OS
  ( baseDir 
  , isSingleWord
  , toLowerOs
  ) where

-- Constant values that are OS-Specific
import System.OsPath (OsPath)
-- import "os-string" System.OsString 
-- import "os-string" System.OsString qualified as OSS

import System.OsString.Compat (OsString, OsChar, osstr, toChar, unsafeFromChar)

import Data.Word

import Data.Char qualified as C

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.OsString.Internal.Types.Compat (unWW, getOsChar)
#else
import System.OsString.Internal.Types.Compat (unPW, getOsChar)
#endif

-- | Simple "root" path to be used as a default.
--   On Windows, it becomes `C:\\`. Otherwise,
--   it just becomes `/`.
baseDir :: OsPath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
baseDir = [osstr|C:\|]
#else
baseDir = [osstr|/|]
#endif

-- | Check whether an `OsChar` is part of a 
--   multi-word `Char` or not.
isSingleWord :: OsChar -> Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isSingleWord c = cx <= 0xD7FF || cx >= 0xE000
  where cx = unWW $ getOsChar c
#else
isSingleWord c = cx < 128
  where cx = unPW $ getOsChar c
#endif

-- | Check whether a `Char` fits into an `OsChar`.
isSingleChar :: Char -> Bool
isSingleChar c
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  = cx <= 0xD7FF || (cx >= 0xE000 && cx <= 0xFFFF)
#else
  = cx <= 0x7F
#endif
  where cx = C.ord c


-- | Make an `OsChar` lowercase if
--   it falls within the single-word
--   code range of the platform.
toLowerOs :: OsChar -> OsChar
toLowerOs c
  | not (isSingleWord c) = c
  | C.isLower cc         = c
  | isSingleChar cl      = cm
  | otherwise            = c
  where 
    cc = toChar c
    cl = C.toLower cc
    cm = unsafeFromChar cl
