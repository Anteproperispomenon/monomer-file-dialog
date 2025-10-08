{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Monomer.Widgets.Extra.FileDialog.OS
  ( baseDir ) where

-- Constant values that are OS-Specific
import System.OsPath
import System.OsString (osstr)

-- | Simple "root" path to be used as a default.
--   On Windows, it becomes `C:\\`. Otherwise,
--   it just becomes `/`.
baseDir :: OsPath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
baseDir = [osstr|C:\|]
#else
baseDir = [osstr|/|]
#endif
