module SoundChange (
  SoundChange,
  applySoundChange
) where

import Data.List (isInfixOf)
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
      getInitial = (!!1)  :: [String] -> String
      getFinal = (!!2)    :: [String] -> String
      getCond = (!!3)     :: [String] -> String

      splitstr :: [String]
      splitstr = splitOneOf ">/" str

{- |
  Applies the given SoundChange to the given String
-}
applySoundChange ::
  SoundChange -- ^ The sound change rule to apply
  -> Map Char SoundGroup -- ^ Map of SoundGroups to use
  -> String -- ^ String to apply sound change rule to
  -> String

applySoundChange sc sgs =
  tail.init $ apply 0 sc modstr
  where
    modstr :: String
    modstr = "#" ++ map toLower str ++ "#"

    apply :: Int -> String -> String
    apply _ "" = ""
    apply pos str =
      if initial sc == take len str
        then applicable (when sc)
