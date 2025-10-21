
module Monomer.Widgets.Extra.FileDialog.Filters.Internal
  (

  ) where

import System.OsPath
import System.Directory.OsPath

import Data.Text qualified as T

import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))

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
data FileFormat
  = FileFormat (NonEmpty OsString) (NonEmpty OsString) T.Text
  deriving (Show, Eq)

-- Note that equality here is not true equality;
-- ".jpg" and "jpg" would refer to the same extension.

-- Note: Not using "FileType" as a name since
-- that term is a bit ambiguous; it could refer
-- to format (e.g. jpeg vs png) or "kind"
-- (e.g. video vs picture).

-- | A "Kind" of a file, e.g. video/text/picture,
--   which can include multiple extensions.
-- data FileKind




