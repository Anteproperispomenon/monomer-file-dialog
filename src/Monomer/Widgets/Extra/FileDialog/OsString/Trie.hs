module Monomer.Widgets.Extra.FileDialog.OsString.Trie
  ( ExtTrie(..)
  , populateExtTrie
  , populateExtTrieFP
  ) where

import Data.Trie.Set qualified as TS

import System.OsPath (OsPath)
import System.OsPath qualified as OSP

import Monomer.Widgets.Extra.FileDialog.OsString.Compat (OsString, OsChar)
import Monomer.Widgets.Extra.FileDialog.OsString.Compat qualified as OSS

import Monomer.Widgets.Extra.FileDialog.OS (toLowerOs)

import Data.Maybe

import Data.Coerce

import Data.ByteString.Short qualified as BSS

-- | A trie containing a set of (backwards!) 
--   extensions. These can then be used to
--   read back through an `OsPath` to see
--   if it matches one of the extensions. 
newtype ExtTrie = ExtTrie { unwrapExtTrie :: TS.TSet OsChar } deriving (Eq)

-- Reverse the extensions as you show them.
instance Show ExtTrie where
    show (ExtTrie tr) = "populateExtTrie " ++ show (mapMaybe (OSP.decodeUtf . OSP.pack . reverse) (TS.toList tr))

-- | Add a bunch of extensions to an `ExtTrie`.
--   Note that it will automatically lower-case
--   and reverse the extensions as it inserts them.
populateExtTrie :: [OsString] -> ExtTrie
populateExtTrie [] = ExtTrie TS.empty
populateExtTrie strs = ExtTrie (TS.fromList $ map (reverse . OSP.unpack) strs)

populateExtTrieFP :: [String] -> ExtTrie
populateExtTrieFP strs = populateExtTrie (mapMaybe OSP.encodeUtf strs) 


