{- |
Module      : Util
Description : Utility functions for Yasgheld
Copyright   : (c) Andrew Ray, 2017
License     : MIT

Utility functions for Yasgheld.
-}
module Util (
  strip,
  replace,
  removeComment,
  (??),
  (?)
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, isSubsequenceOf)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)

-- | Strips leading and trailing whitespace from the given String.
strip :: String -- ^ Input String
      -> String -- ^ Output String, whitespace removed
strip = dropWhileEnd isSpace.dropWhile isSpace

-- | Performs substitution on a string
replace :: Eq a
        => [a] -- ^ The search value
        -> [a] -- ^ The replace value
        -> [a] -- ^ The string to search in
        -> [a] -- ^ The string, after substitutions
replace [] _ str = str
replace _ _ [] = []
replace find repl str
  | find `isPrefixOf` str =
    repl ++ replace find repl (drop (length find) str)
  | otherwise =
    head str : replace find repl (tail str)

-- | Removes comments from strings
removeComment :: String -- ^ String to remove comments from
              -> String -- ^ String with comments removed
removeComment str
  | ";" `isSubsequenceOf` str = head $ splitOn ";" str
  | otherwise      = str

-- | Finds the list referenced by the given first term
infixl 9 ?
(?) :: (Eq a)
    => [(a, b, c)] -- ^ The list of three-tuples to search in
    -> a -- ^ The first term to search for
    -> Maybe (b, c) -- ^ Just the last two terms, or Nothing if no matches
(?) [] _ = Nothing
(?) ((a, b, c):rest) find
  | a == find = Just (b, c)
  | otherwise = rest ? find

-- | Finds the second term referenced by the first given term
infixl 9 ??
(??) :: (Eq a)
     => [(a, b, c)] -- ^ The list of three-tuples to search in
     -> a -- ^ The first term to search for
     -> Maybe b -- ^ Just the second term, or Nothing if no matches
(??) list find
  | isNothing (list ? find) = Nothing
  | otherwise = Just $ (\(Just (b, _)) -> b) (list ? find)
