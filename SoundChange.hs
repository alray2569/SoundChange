{- |
  Module contains the information pertaining to SoundChanges.

  Read syntax:

  @
  in > out / condition
  @

  where @in@ is the input string, @out@ is the output string, @condition@
  is the <Condition> of application. In the event that @out@ or @in@ is empty,
  the symbol @{}@ may be used, as in @h > {} / V_@ to indicate loss of
  syllable-final \/h\/.
-}
module SoundChange (
  SoundChange,
  applySoundChange,
  initial,
  final,
  when
) where

import Data.List.Split
import Data.Char (toLower)
import Data.Map (Map)

import Condition
import SoundGroup
import Util (strip, matches)

{- |
  The representation of a sound change.
-}
data SoundChange = SoundChange {
  initial :: String, -- ^ Returns the initial value, before the sound change
  final :: String, -- ^ Returns the final value, after the sound change
  when :: Condition -- ^ Returns the condition under which the rule applies
} deriving (Show)

instance Read SoundChange where
  readsPrec _ str
    | hasCond str =
      [(SoundChange {
        initial = getInitial splitstr,
        final = if getFinal splitstr /= "{}"
          then getFinal splitstr
          else "",
        when = read $ getCond splitstr
      }, "")]

    | otherwise   =
      [(SoundChange {
        initial = getInitial splitstr,
        final = getFinal splitstr,
        when = Always
      }, "")]

    where
      hasCond = elem '/'  :: String   -> Bool
      getInitial = (!!0)  :: [String] -> String
      getFinal = (!!1)    :: [String] -> String
      getCond = (!!2)     :: [String] -> String

      splitstr :: [String]
      splitstr = map strip $ splitOneOf "/>" str

{- |
  Applies the given SoundChange to the given String
-}
applySoundChange ::
  SoundChange -- ^ The sound change rule to apply
  -> Map Char SoundGroup -- ^ Map of SoundGroups to use
  -> String -- ^ String to apply sound change rule to
  -> String -- ^ String with sound change applied to it

applySoundChange (SoundChange input output cond) sgs string =
  tail.init $ apply 0 modstr
  where
    modstr = "#" ++ map toLower string ++ "#" :: String

    apply :: Int -> String -> String
    apply _ "" = ""
    apply pos str
      | and (zipWith (matches sgs) input (take len str)) &&
        applicable cond sgs modstr pos =
          output ++ apply (pos + len) (drop len str)
      | otherwise = recurse
        where
          len = length input :: Int
          recurse = head str : apply (pos + 1) (tail str) :: String
