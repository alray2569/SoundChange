{- |
  Utility functions for Yasgheld.
-}
module Util (
  strip,
  matches
) where

import Data.Char (isSpace, isLower)
import Data.List (dropWhileEnd)
import Data.Map (Map, (!))

{- |
  Strips leading and trailing whitespace from the given String.
-}
strip :: String -- ^ Input String
      -> String -- ^ Output String, whitespace removed
strip = dropWhileEnd isSpace.dropWhile isSpace -- remove spaces around

{- |
  Determines if the given characters match after group resolution.
-}
matches :: Map Char String
        -> Char
        -> Char
        -> Bool
matches _ '_' _ = True
matches _ _ '*' = True
matches sgs pchar achar =
  if isGroup pchar
    then achar `elem` (sgs ! pchar)
    else pchar == achar
  where
    isGroup :: Char -> Bool
    isGroup char = not (isLower char || char == '_' || char == '#')
