module SoundChange (
  SoundChange,
  applySoundChange
) where

import Data.List.Split
import Data.Char (toLower)
import Data.Map (Map)

import Condition
import SoundGroup

{- |
  The representation of a sound change.
-}
data SoundChange = SoundChange {
  initial :: String, -- ^ The initial value, before the sound change
  final :: String, -- ^ The final value, after the sound change
  when :: Condition -- ^ The condition under which the rule applies
} deriving (Show)

instance Read SoundChange where
  readsPrec _ str
    | hasCond str =
      [(SoundChange {
        initial = getInitial splitstr,
        final = getFinal splitstr,
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
      splitstr = splitOneOf "/>" str

{- |
  Applies the given SoundChange to the given String
-}
applySoundChange ::
  SoundChange -- ^ The sound change rule to apply
  -> Map Char SoundGroup -- ^ Map of SoundGroups to use
  -> String -- ^ String to apply sound change rule to
  -> String

applySoundChange sc sgs string =
  tail.init $ apply 0 modstr
  where
    modstr = "#" ++ map toLower string ++ "#" :: String

    apply :: Int -> String -> String
    apply _ "" = ""
    apply pos str
      | initial sc == take (length $ initial sc) str =
        if applicable (when sc) sgs modstr pos
          --then final sc ++ drop (length $ initial sc) str
          then final sc ++ apply (pos + len) (drop len str)
          else recurse
      | otherwise = recurse
        where
          len = length $ initial sc :: Int
          recurse = head str : apply (pos + 1) (tail str) :: String
