{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Monomer.Widgets.Extra.FileDialog.OsString.Compat
  ( OsString
  , OsChar
  , unsnoc
  , uncons
  , indexMaybe
  , length
  , null
  , take
  , unWW
  , unPW
  , getOsChar
  , osstr
  , toChar
  , unsafeFromChar
  ) where

import Prelude hiding (length, unsnoc, uncons, index, take, null)

#if MIN_VERSION_filepath(1,5,0)
import "os-string" System.OsString.Internal.Types 
  ( unWW
  , unPW
  , getOsChar
  , getOsString
  , pattern WS
  , pattern PS
  , getWindowsString
  , getPosixString
  , pattern WW
  , pattern PW
  , toChar
  , unsafeFromChar
  )
import "os-string" System.OsString (OsString, OsChar, osstr, unsnoc, uncons, indexMaybe, length, null, take, osstr)
import "os-string" System.OsString qualified as OSS
import "os-string" System.OsString qualified as OS2
import "os-string" System.OsString.Internal.Types qualified as OS2
#else
import "filepath" System.OsString.Internal.Types (unWW, unPW, getOsChar, getOsString, pattern WS, pattern PS, getWindowsString, getPosixString, pattern WW, pattern PW)
import "filepath" System.OsString.Internal.Types qualified as OSS
import "filepath" System.OsString (OsString, OsChar, osstr)
import "filepath" System.OsString qualified as OSS
import "os-string" System.OsString qualified as OS2
import "os-string" System.OsString.Internal.Types qualified as OS2
import Data.Coerce

indexMaybe :: OSS.OsString -> Int -> Maybe OSS.OsChar
indexMaybe = coerce OS2.indexMaybe
-- indexMaybe ostr n = coerce <$> (OS2.indexMaybe (coerce ostr) n)

unsnoc :: OSS.OsString -> Maybe (OSS.OsString, OSS.OsChar)
unsnoc = coerce OS2.unsnoc

uncons :: OSS.OsString -> Maybe (OSS.OsChar, OSS.OsString)
uncons = coerce OS2.uncons

null :: OSS.OsString -> Bool
null = coerce OS2.null

length :: OSS.OsString -> Int
length = coerce OS2.length

take :: Int -> OSS.OsString -> OSS.OsString
take = coerce OS2.take

toChar :: OSS.OsChar -> Char
toChar = coerce OS2.toChar

unsafeFromChar :: Char -> OSS.OsChar
unsafeFromChar = coerce OS2.unsafeFromChar

#endif

coerceOS2 :: OSS.OsString -> OS2.OsString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
coerceOS2 (OSS.OsString (WS str)) = OS2.OsString (OS2.WS str)
#else
coerceOS2 (OSS.OsString (PS str)) = OS2.OsString (OS2.PS str)
#endif

coerceOS1 :: OS2.OsString -> OSS.OsString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
coerceOS1 (OS2.OsString (OS2.WS str)) = OSS.OsString (OSS.WS str)
#else
coerceOS1 (OS2.OsString (OS2.PS str)) = OSS.OsString (OSS.PS str)
#endif

coerceOW2 :: OSS.OsChar -> OS2.OsChar
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
coerceOW2 (OSS.OsChar (WW str)) = OS2.OsChar (OS2.WW str)
#else
coerceOW2 (OSS.OsChar (PW str)) = OS2.OsChar (OS2.PW str)
#endif

coerceOW1 :: OS2.OsChar -> OSS.OsChar
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
coerceOW1 (OS2.OsChar (OS2.WW str)) = OSS.OsChar (OSS.WW str)
#else
coerceOW1 (OS2.OsChar (OS2.PW str)) = OSS.OsChar (OSS.PW str)
#endif



