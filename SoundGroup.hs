{-# LANGUAGE Safe #-}

{- |
Module      : SoundGroup
Description : SoundGroup and environs
Copyright   : (c) Andrew Ray, 2017
License     : MIT

SoundGroup and environs

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
  parseSoundGroup,
  matches
) where

import Data.List.Split (splitOn)
import Data.Char (chr)

import Util (strip, (??))

-- | A SoundGroup is a group of sounds, to be identified by a
--  capital letter as a standin for any one of those sounds.
type SoundGroup = (Char, String, Maybe String)

-- | Parses a string into a Map Char SoundGroup
parseSoundGroup :: Int -- ^ The number to assign to this SoundGroup
                -> String -- ^ The string to parse
                -> SoundGroup -- ^ The parsed SoundGroup Tuple
parseSoundGroup num string
  | length first == 1 = (head first, strip $ split !! 1, Nothing)
  | otherwise = (chr $ num + 7935, strip $ split !! 1, Just first)
  where
    split = splitOn ":" string
    first = strip $ head split

-- | Determines if the given characters match after group resolution.
matches :: [SoundGroup] -- ^ The SoundGroups to check in
        -> Char -- ^ The Character to check against
        -> Char -- ^ The Character to check
        -> Bool -- ^ True iff the characters match.
matches _ '_' _ = True
matches _ '*' _  = True
matches sgs pchar achar =
  maybe (achar == pchar) (elem achar) (sgs ?? pchar)
