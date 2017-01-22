{- |
  Utility functions for Yasgheld.
-}
module Util (
  strip,
  matches
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (isNothing)

type SoundGroup = (Char, String, Maybe String)

{- |
  Strips leading and trailing whitespace from the given String.
-}
strip :: String -- ^ Input String
      -> String -- ^ Output String, whitespace removed
strip = dropWhileEnd isSpace.dropWhile isSpace -- remove spaces around

{- |
  Determines if the given characters match after group resolution.
-}
matches :: [SoundGroup] -- ^ The SoundGroups to check in
        -> Char -- ^ The Character to check against
        -> Char -- ^ The Character to check
        -> Bool -- ^ True iff the characters match.
matches _ '_' _ = True
matches _ '*' _  = True
matches sgs pchar achar =
  maybe (achar == pchar) (elem achar) (sgs ?? pchar)

infixl 9 ??
(??) :: (Eq a) => [(a, b, c)] -> a -> Maybe b
(??) list find
  | isNothing (list ? find) = Nothing
  | otherwise = Just $ (\(Just (b, _)) -> b) (list ? find)

infixl 9 ?
(?) :: (Eq a) => [(a, b, c)] -> a -> Maybe (b, c)
(?) [] _ = Nothing
(?) ((a, b, c):rest) find
  | a == find = Just (b, c)
  | otherwise = rest ? find

infixl 9 >>??
(>>??) :: (Eq c) => [(a, b, c)] -> c -> Maybe a
(>>??) list find
  | isNothing (list >>? find) = Nothing
  | otherwise = Just $ (\(Just (a, _)) -> a) (list >>? find)

infixl 9 >>?
(>>?) :: (Eq c) => [(a, b, c)] -> c -> Maybe (a, b)
(>>?) [] _ = Nothing
(>>?) ((a, b, c):rest) find
  | c == find = Just (a, b)
  | otherwise = rest >>? find
