{- |
  Module contains information about SoundGroups
-}
module SoundGroup (
  SoundGroup,
  parseSoundGroup
) where

import Data.List.Split (splitOn)

import Util (strip)

{- |
  A SoundGroup is a group of sounds, to be identified by a
  capital letter as a standin for any one of those sounds.
-}
type SoundGroup = String

{- |
  Parses a string into a Map Char SoundGroup
-}
parseSoundGroup :: String -> (Char, SoundGroup)
parseSoundGroup string =
  (head $ strip $ head split :: Char, strip $ split !! 1)
  where
    split = splitOn ":" string
