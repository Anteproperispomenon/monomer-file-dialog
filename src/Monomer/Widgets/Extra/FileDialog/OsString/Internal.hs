{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}

module Monomer.Widgets.Extra.FileDialog.OsString.Internal
  ( UnsnocOsString(.., UnsnocOsStringW)
  , null
  , unsnoc
  ) where

import Prelude hiding (null, unsnoc)

-- import "os-string" System.OsString qualified as OSS
-- import "os-string" System.OsString (OsString, OsChar)

import System.OsString.Compat qualified as OSS
import System.OsString.Compat (OsString, OsChar)

-- | A wrapper over @(`OsString`, `Int`)@ to
--   effectively allow `OSS.unsnoc` without
--   needing to constantly copy over bytes.
newtype UnsnocOsString = UnsnocOsString' {getUnsnocOsStr :: (OSS.OsString, Int)} deriving (Eq)

-- Hmm...
pattern UnsnocOsStringW str <- ((\(UnsnocOsString' (ostr, n)) -> OSS.take (n+1) ostr) -> str)
  where UnsnocOsStringW str = UnsnocOsString' (str, ((OSS.length str) - 1))

instance Show UnsnocOsString where
  show (UnsnocOsString' (ostr, n)) = show (OSS.take (n+1) ostr)

-- | Check if the underlying int has reached (-1)
--   or the string is empty.
null :: UnsnocOsString -> Bool
null (UnsnocOsString' (ostr, n))
  = (n <= (-1)) || (OSS.null ostr)

unsnoc :: UnsnocOsString -> Maybe (UnsnocOsString, OsChar)
unsnoc (UnsnocOsString' (ostr, n))
  | n <= (-1) = Nothing
  | (Just c) <- OSS.indexMaybe ostr n
  = Just ((UnsnocOsString' (ostr, n-1)), c)
  | otherwise = Nothing
