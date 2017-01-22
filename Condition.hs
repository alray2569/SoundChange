{- |
  Module contains information relating to conditions and situations.

  Read syntax:

  @
  [!]pre_post [& [!]pre_post [& [!]pre_post ...]]
  @

  where @pre@ is the precondition and @post@ is the postcondition

  @!@ is used for negation,
  @&@ is used to combine conditions,
  @#@ is used for beginning or end of word,
  @_@ is used for search pattern characters
-}
module Condition (
  Situation,
  Condition(Always, And, If, IfNot),
  applicable
) where

import Data.List (elemIndex)
import Data.List.Split
import Control.Monad (join)

import SoundGroup
import Util (strip)

{- |
  A situation, written out as specified in README:

  * #: beginning or end of word
  * _: replacement characters
-}
type Situation = String

{- |
  A condition is one of Always, a pairing of conditions,
  a situation, or a negated situation, as specified in README:

  * !: negates following condition
  * &: combines two conditions
-}
data Condition =
  Always | -- ^ Always applicable
  Condition `And` Condition | -- ^ Applicable if both are applicable
  If Situation | -- ^ Applicable if true
  IfNot Situation -- ^ Applicable if false

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
  -> [SoundGroup] -- ^ Map of SoundGroups to use
  -> String -- ^ The example to check
  -> Int    -- ^ The position to check in the example
  -> Bool   -- ^ True iff the Condition is applicable in this context

applicable Always _ _ _ = True

applicable (cond1 `And` cond2) sgs str pos =
  applicable cond1 sgs str pos && applicable cond2 sgs str pos

applicable (IfNot cond) sgs str pos =
  not $ applicable (If cond) sgs str pos

applicable (If cond) sgs str pos =
  let
    baseIndex = elemIndex '_' cond :: Maybe Int
    startIndex = maybe 0 (pos -) baseIndex :: Int
    checkArea = take (length cond) (drop startIndex str) :: String
  in
    and $ zipWith (matches sgs) cond checkArea
