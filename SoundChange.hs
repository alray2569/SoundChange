{-# LANGUAGE Safe #-}

{- |
Module      : SoundChange
Description : SoundChange datatype and environs
Copyright   : (c) Andrew Ray, 2017
License     : MIT

SoundChange datatype and environs

Read syntax:

@
in > out / condition
@
(Sound in changes to out when condition is true)

@
'\@ n / condition
@
(Stress moves to nth syllable when condition is true)

@
'\@ -n / condition
@
(Stress moves to nth syllable from end when condition is true)

@
'> n / condition
@
(Stress moves n syllables towards end when condition is true)

@
'> -n / condition
@
(Stress moves n syllables towards front when condition is true)

where @in@ is the input string, @out@ is the output string, @condition@
is the <Condition> of application. In the event that @out@ or @in@ is empty,
the symbol @{}@ may be used, as in @h > {} / V_@ to indicate loss of
syllable-final \/h\/.
-}
module SoundChange (
  SoundChange(SoundChange, StressMove, StressShift),
  applySoundChange,
  initial,
  final,
  when
) where

import Data.List.Split
import Data.Char (toLower)

import Condition
import SoundGroup
import Util (strip, replace)

-- | The representation of a sound change.
data SoundChange =
  SoundChange {
    initial :: String, -- ^ Returns the initial value, before the sound change
    final :: String, -- ^ Returns the final value, after the sound change
    when :: Condition -- ^ Returns the condition under which the rule applies
  } |
  StressMove {
    moveTo :: Int, -- ^ The syllable to move stress to,
    when :: Condition -- ^ Condition under which the rule applies
  } |
  StressShift {
    move :: Int, -- ^ The amount to move the stress by
    when :: Condition
  }
  deriving (Eq)

instance Read SoundChange where
  -- StressShift rule
  readsPrec _ ('\'' : '>' : rest)
    | hasCond =
      let [amt, cond] = splitstr
      in [(StressShift (read amt) (read cond), "")]
    | otherwise =
      [(StressShift (read $ strip rest) Always, "")]
    where
      splitstr = map strip $ splitOneOf "/>" rest :: [String]
      hasCond = elem '/' rest :: Bool

  -- StressMove rule
  readsPrec _ ('\'' : '@' : rest)
    | hasCond =
      let [num, cond] = splitstr
      in [(StressMove (read $ strip num) (read cond), "")]
    | otherwise =
      [(StressMove (read $ strip rest) Always, "")]
    where
      splitstr = map strip $ splitOneOf "/>" rest :: [String]
      hasCond = elem '/' rest :: Bool

  -- SoundChange rule
  readsPrec _ str
    | hasCond =
      let [ini, fin, cond] = splitstr
      in [(SoundChange ini
            (if fin /= "{}"
                then fin
                else "")
            (read cond), "")]
    | otherwise =
      let [ini, fin] = splitstr
      in [(SoundChange ini fin Always, "")]
    where
      splitstr = map strip $ splitOneOf "/>" str :: [String]
      hasCond = elem '/' str :: Bool

instance Show SoundChange where
  show sc
    | hasCond = initial sc ++ " > " ++ final sc ++ " / " ++ show (when sc)
    | otherwise = initial sc ++ " > " ++ final sc
    where hasCond = when sc /= Always :: Bool

-- | Applies the given SoundChange to the given String
applySoundChange :: SoundChange -- ^ The sound change rule to apply
                 -> [SoundGroup] -- ^ Map of SoundGroups to use
                 -> String -- ^ String to apply sound change rule to
                 -> String -- ^ String with sound change applied to it

applySoundChange (SoundChange input output cond) sgs string =
  tail.init $ apply 0 modstr
  where
    modstr = "#" ++ map toLower string ++ "#" :: String

    apply :: Int -> String -> String
    apply _ "" = ""
    apply pos str
      | allMatch && allApply = output ++ apply (pos + len) (drop len str)
      | otherwise = head str : apply (pos + 1) (tail str)
      where
        len = length input :: Int
        allMatch = and (zipWith (matches sgs) input (take len str)) :: Bool
        allApply = applicable cond sgs (replace input "_" modstr) pos :: Bool
