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

type Situation = String

data Condition =
  Always |
  Condition `And` Condition |
  If Situation |
  IfNot Situation

instance Read Condition where
  readsPrec _ str
    | strip str == "Always" =
      [(Always, "")]
    | strip str == "_" =
      [(Always, "")]
    | ispair str =
      [(And (read $ head conds) (read $ join $ tail conds), "")]
    | isneg str =
      [(IfNot $ tail str, "")]
    | otherwise =
      [(If str, "")]
    where
      ispair = elem '&'
      conds = map strip $ splitOn "&" str
      isneg elt = head elt == '!'
      strip = dropWhileEnd isSpace.dropWhile isSpace

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
