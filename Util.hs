{- |
  Utility functions for Yasgheld.
-}
module Util (
  strip
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

{- |
  Strips leading and trailing whitespace from the given String.
-}
strip :: String -- ^ Input String
      -> String -- ^ Output String, whitespace removed
strip = dropWhileEnd isSpace.dropWhile isSpace -- remove spaces around
