module Condition (
  Situation,
  Condition(Always, And, If, IfNot),
  applicable
) where

import Data.Char (isSpace, isLower)
import Data.List (dropWhileEnd, elemIndex)
import Data.List.Split
import Data.Map (Map, (!))
import Control.Monad (join)

import SoundGroup

{- |
  A situation, written out as specified in README:
  #: beginning or end of word
  _: replacement characters
-}
type Situation = String

{- |
  A condition is one of Always, a pairing of conditions,
  a situation, or a negated situation, as specified in README:
  !: negates following condition
  &: combines two conditions
-}
data Condition =
  Always |
  Condition `And` Condition |
  If Situation |
  IfNot Situation

instance Read Condition where
  readsPrec _ str
    -- Always parse "Always" as Always
    | strip str == "Always" = [(Always, "")]
    -- Always parse "_" as Always
    | strip str == "_" = [(Always, "")]
    -- Evaluate ands separately and combine
    | ispair str = [(And (read $ head conds) (read $ join $ tail conds), "")]
    -- Evaluate not, removing the '!', and spaces around
    | isneg str = [(IfNot $ tail $ strip str, "")]
    -- If is trivial, just add If to the string and remove spaces around
    | otherwise = [(If $ strip str, "")]
    where
      ispair = elem '&' -- check if contains pairing
      conds = map strip $ splitOn "&" str -- split on pairing
      isneg elt = head elt == '!' -- check if contains negation
      strip = dropWhileEnd isSpace.dropWhile isSpace -- remove spaces around

-- inverse of Read
instance Show Condition where
  show (cond1 `And` cond2) = show cond1 ++ " & " ++ show cond2
  show (If situation) = situation
  show (IfNot situation) = "!" ++ situation
  show Always = "Always"

{-|
  Determines if the condition applies to the given example
  at the suggested location.
-}
applicable ::
  Condition -- ^ The condition to check against
  -> Map Char SoundGroup -- ^ Map of SoundGroups to use
  -> String -- ^ The example to check
  -> Int    -- ^ The position to check in the example
  -> Bool

applicable Always _ _ _ = True

applicable (cond1 `And` cond2) sgs str pos =
  applicable cond1 sgs str pos && applicable cond2 sgs str pos

applicable (IfNot cond) sgs str pos =
  not $ applicable (If cond) sgs str pos

applicable (If cond) sgs str pos =
  and $ zipWith matches cond checkArea
  where
    baseIndex :: Maybe Int
    baseIndex = elemIndex '_' cond

    startIndex :: Int
    startIndex = maybe 0 (pos -) baseIndex

    isGroup :: Char -> Bool
    isGroup = not.isLower

    matches :: Char -> Char -> Bool
    matches '_' _ = True
    matches pchar achar =
      if isGroup pchar
        then achar `elem` (sgs ! pchar)
        else pchar == achar

    checkArea :: String
    checkArea = take (length cond) (drop startIndex str)
