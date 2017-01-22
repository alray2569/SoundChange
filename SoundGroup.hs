{- |
  Module contains information about SoundGroups.

  Read Syntax:

  @
  G: abcd
  @

  where @G@ is the symbol to represent the group, and @abcd@ are the sounds
  in the group. In the future, the alternative syntax:
  @
  GroupName: abcd
  @
  is expected to work, where GroupName is any multicharacter identifier for
  the group.
-}
module SoundGroup (
  SoundGroup,
  parseSoundGroup
) where

import Data.List.Split (splitOn)
import Data.Char (chr, ord)

import Util (strip)

{- |
  A SoundGroup is a group of sounds, to be identified by a
  capital letter as a standin for any one of those sounds.
-}
type SoundGroup = (Char, String, Maybe String)

{- |
  Parses a string into a Map Char SoundGroup
-}
parseSoundGroup :: Int -- ^ The number to assign to this SoundGroup
  -> String -- ^ The string to parse
  -> SoundGroup -- ^ The parsed SoundGroup Tuple
parseSoundGroup num string
  | length first == 1 = (head first, strip $ split !! 1, Nothing)
  | otherwise = (chr $ num + ord 'z', strip $ split !! 1, Just first)
  where
    split = splitOn ":" string
    first = strip $ head split
