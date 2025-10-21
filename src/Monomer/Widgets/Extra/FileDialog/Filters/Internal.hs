
module Monomer.Widgets.Extra.FileDialog.Filters.Internal
  ( FileFormat(..)
  , FileKind(..)
  , kindToTrie
  ) where

import System.OsPath
import System.Directory.OsPath

import Data.Text qualified as T

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

import Monomer.Widgets.Extra.FileDialog.OsString.Trie

import Data.Semigroup (sconcat)

-- Important Functions

-- isExtensionOf
-- (use to filter extension type)
-- (Note that it works regardless of whether
--  you include the "." before the extension
--  or not)
-- The downside is it fails when the case of
-- the extension differs. e.g. 
-- @".jpeg" `isExtensionOf` "ASDF.JPEG" == False@
-- Unfortunately, there's not really a safe way to
-- deal with this, thanks to the way `OsPath`s work.

-- | A file type that contains a specific filetype, which may
--   have more than one possible extension, e.g. ".jpg" or ".jpeg".
data FileFormat = FileFormat 
  { ffExts :: (NonEmpty OsString)
  -- , ffShowExts :: (NonEmpty OsString) -- Extensions to be shown (shorter than ffExts)
  , ffName :: T.Text
  } deriving (Show, Eq)

-- Note that equality here is not true equality;
-- ".jpg" and "jpg" would refer to the same extension.

-- Note: Not using "FileType" as a name since
-- that term is a bit ambiguous; it could refer
-- to format (e.g. jpeg vs png) or "kind"
-- (e.g. video vs picture).

-- | A "Kind" of a file, e.g. video/text/picture,
--   which can include multiple extensions.
data FileKind = FileKind 
  { fkName    :: T.Text 
  , fkFormats :: (NonEmpty FileFormat)
  } deriving (Show, Eq)

-- | Make a trie out of a `FileKind`
kindToTrie :: FileKind -> ExtTrie
kindToTrie (FileKind _ fmts) = populateExtTrie $ NE.toList $ sconcat $ fmap ffExts fmts

fmtToTrie :: FileFormat -> ExtTrie
fmtToTrie (FileFormat exts _) = populateExtTrie $ NE.toList exts

-- | Processed data about a filter.
data FilterData = FilterData
  { fltName :: T.Text
  , fltTrie :: ExtTrie
  } deriving (Show, Eq)

allFiles :: FilterData
allFiles = FilterData "All Files" anythingTrie

-- | Convert a `FileKind` into a list of potential
--   filters. If a `FileKind` has only one format,
--   only one `FilterData` is produced. Otherwise,
--   it produces one for the `FileKind` as a whole,
--   plus one for each `FileFormat` in the `FileKind`.
makeFilterData' :: FileKind -> [FilterData]
makeFilterData'    (FileKind nom (fmt :| []))
  = [FilterData nom (fmtToTrie fmt)]
makeFilterData' fk@(FileKind nom fmts)
  = (FilterData nom (kindToTrie fk)) : (map (\ff@(FileFormat _ nom) -> FilterData nom (fmtToTrie ff)) (NE.toList fmts))

