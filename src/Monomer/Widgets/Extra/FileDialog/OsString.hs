-- | You probably shouldn't use this module.
-- 
--   Unfortunately, after reading through the code for
--   `OsString` etc, it seems they don't account for
--   multi-word characters in UTF-16 or multi-byte
--   characters in UTF-8. i.e. an `OsChar` isn't
--   really a character; it's a byte (POSIX) or
--   Word16 (Windows) that's either a character
--   or a piece of a character. It works comparing
--   them one-by-one for plain equality, since the
--   same bytes/words should occur in the same order,
--   but if you try to treat them like `Char`s, using
--   `C.toLower` etc, you won't be taking the whole character.
--   
--   I was gonna just mark this module as deprecated and say
--   not to use it, but then I realised I could check bytes
--   to see whether they're part of a multi-word character
--   or not, and then just leave them alone if so. 

module Monomer.Widgets.Extra.FileDialog.OsString
  ( isUncasedExtensionOf
  -- , isSomeExtensionOf
  ) where

import Data.ByteString.Short qualified as BS

-- import System.OsString qualified as OSS
-- import System.OsString (OsString, OsChar)

import System.OsPath (OsPath)

import Monomer.Widgets.Extra.FileDialog.OsString.Compat qualified as OSS
import Monomer.Widgets.Extra.FileDialog.OsString.Compat (OsString, OsChar)

import Data.Char qualified as C

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Monomer.Widgets.Extra.FileDialog.OsString.Internal qualified as UO

import Monomer.Widgets.Extra.FileDialog.OS (isSingleWord)

-- | Does the given filename have the specified extension?
--   Ignores casing of the extension. Based on the code from
--   "System.OsPath".
--
-- > "png" `isExtensionOf` "/directory/file.PNG" == True
-- > ".Png" `isExtensionOf` "/directory/file.png" == True
-- > ".TAR.gz" `isExtensionOf` "bar/foo.tar.gz" == True
-- > "ar.gz" `isExtensionOf` "bar/foo.tar.gz" == False
-- > "png" `isExtensionOf` "/directory/file.png.jpg" == False
-- > "csv/table.csv" `isExtensionOf` "/data/csv/table.csv" == False
{- isUncasedExtensionOf :: BS.ShortByteString -> BS.ShortByteString -> Bool
isUncasedExtensionOf ext = \fp -> case uncons ext of
  Just (x, _)
    | x == _period -> isSuffixOf ext . takeExtensions $ fp
  _ -> isSuffixOf (_period `cons` ext) . takeExtensions $ fp
-}

-- | Check if any file extension applies to a given
--   path/file.
-- isSomeExtensionOf :: NonEmpty OsString -> OsPath -> Bool


-- To be called by `isSomeExtension`/etc...
-- (Deprecated?)
checkUncasedExtension :: OsString -> OsString -> Bool
checkUncasedExtension ext1 ext2
  | Just (c1,rst1) <- OSS.uncons ext1
  , Just (c2,rst2) <- OSS.uncons ext2
  = if (c1 == c2) 
    -- Check the rest if 
    then (checkUncasedExtension rst1 rst2)
    else if ((C.toLower $ OSS.toChar c1) == (C.toLower $ OSS.toChar c2))
            then (checkUncasedExtension rst1 rst2)
            else False
  | otherwise = False

_period :: OsChar
_period = OSS.unsafeFromChar '.'

-- | Check whether a string has an un
isUncasedExtensionOf :: OsString -> OsPath -> Bool
isUncasedExtensionOf ext pth = checkUncasedExtensionOf (UO.UnsnocOsStringW ext) (UO.UnsnocOsStringW pth)
-- {-# INLINE isUncasedExtensionOf #-}

checkUncasedExtensionOf :: UO.UnsnocOsString -> UO.UnsnocOsString -> Bool
checkUncasedExtensionOf ext pth
  | Just rslt1 <- UO.unsnoc ext
  , Just rslt2 <- UO.unsnoc pth
  = checkUncasedExtensionOf' rslt1 rslt2
  | Nothing          <- UO.unsnoc ext
  , Just (_rst, osC) <- UO.unsnoc pth
  = osC == _period
  | otherwise = False -- I guess?

checkUncasedExtensionOf' :: (UO.UnsnocOsString, OsChar) -> (UO.UnsnocOsString, OsChar) -> Bool
checkUncasedExtensionOf' (rst1, c1) (rst2, c2)
  | (c1 == c2) && (c2 == _period) -- I hope transitivity holds...
  , UO.null rst1 
  = True -- i.e. we got the end of the extension
  | (c1 == c2) = checkUncasedExtensionOf rst1 rst2 -- i.e. continue
  -- Don't check uncased if the word is a part of a multi-word Char.
  | not (isSingleWord c1 && isSingleWord c2) = False
  | (cx1 == cx2) && (cx2 == '.')
  , UO.null rst1 
  = True -- Same as the first guard, but checking for casedness.
  | cx1 == cx2 = checkUncasedExtensionOf rst1 rst2
  | otherwise = False
  where
    -- These are only computed if you get to the third guard.
    -- Thanks Laziness!
    cx1 = C.toLower $ OSS.toChar c1
    cx2 = C.toLower $ OSS.toChar c2



