{- |
  Main module for Yasgheld.
-}
module Main (main) where

import System.Environment (getArgs)
import System.IO
import Data.List (isSubsequenceOf, isPrefixOf)

import SoundChange
import SoundGroup

{- |
  IO Handler for the game
-}
main :: IO ()
main = do
  args <- getArgs

  soundgroupfile <- openFile (fileExt (args !! 2) ".ygg") ReadMode
  sgfcontent <- hGetContents soundgroupfile
  let soundgroups = if ".ygg1" `isSubsequenceOf` (args !! 2)
      then
        map ((\(a,b) -> (a,b, Nothing)).read) (lines sgfcontent)
      else
        zipWith parseSoundGroup [1..] (lines sgfcontent)

  soundchangefile <- openFile (fileExt (args !! 1) ".ygc") ReadMode
  scfcontent <- hGetContents soundchangefile
  let soundchanges = map read (lines $ subGroups soundgroups scfcontent)

  wordlistinfile <- openFile (fileExt (head args) ".ygw") ReadMode
  wlifcontent <- hGetContents wordlistinfile
  let wordlistin = lines wlifcontent

  let wordlistout = applyAllToAll soundchanges soundgroups wordlistin
  writeFile (fileExt (args !! 3) ".ygw") (unlines wordlistout)
  hClose soundchangefile
  hClose wordlistinfile
  hClose soundgroupfile

fileExt :: String -> String -> String
fileExt fn ext =
  if '.' `elem` fn
    then fn
    else fn ++ ext

applyAllToAll :: [SoundChange] -> [SoundGroup] -> [String] -> [String]
applyAllToAll scs sgs =
  map $ foldr ((.).(`applySoundChange` sgs)) id scs

subGroups :: [SoundGroup] -> String -> String
subGroups = foldr ((.).replaceGroup) id

replaceGroup :: SoundGroup -> String -> String
replaceGroup (_, _, Nothing) str = str
replaceGroup (short, _, Just long) str =
  replace ("[" ++ long ++ "]") [short] str

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ str = str
replace _ _ [] = []
replace find repl str
  | find `isPrefixOf` str =
    repl ++ replace find repl (drop (length find) str)
  | otherwise =
    head str : replace find repl (tail str)
