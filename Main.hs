{- |
  Main module for Yasgheld.
-}
module Main (main) where

import System.Environment (getArgs)
import System.IO
import Data.Map (fromList, Map)
import Data.List (isSubsequenceOf)

import SoundChange
import SoundGroup

{- |
  IO Handler for the game
-}
main :: IO ()
main = do
  args <- getArgs

  soundchangefile <- openFile (fileExt (args !! 1) ".ygc") ReadMode
  scfcontent <- hGetContents soundchangefile
  let soundchanges = map read (lines scfcontent)

  wordlistinfile <- openFile (fileExt (head args) ".ygw") ReadMode
  wlifcontent <- hGetContents wordlistinfile
  let wordlistin = lines wlifcontent

  soundgroupfile <- openFile (fileExt (args !! 2) ".ygg") ReadMode
  sgfcontent <- hGetContents soundgroupfile
  let soundgroups = if ".ygg1" `isSubsequenceOf` (args !! 2)
      then
        fromList $ map read $ lines sgfcontent
      else
        fromList $ map parseSoundGroup $ lines sgfcontent

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

applyAllToAll :: [SoundChange] -> Map Char SoundGroup -> [String] -> [String]
applyAllToAll scs sgs =
  map $ foldr ((.).(`applySoundChange` sgs)) id scs
