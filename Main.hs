{- |
Module      : Main
Description : Main module for Yasgheld
Copyright   : (c) Andrew Ray, 2017
License     : MIT

Main module for Yasgheld
-}
module Main (main) where

import System.Environment (getArgs)
import System.IO
import Data.List (isSubsequenceOf)

import SoundChange
import SoundGroup
import Util (replace, removeComment)

{- |
  IO Handler for the game
-}
main :: IO ()
main =
  do { [infile, scfile, sgfile, outfile] <- getArgs -- get filenames

     -- Read the sound group file
     ; soundgroupfile <- openFile (fileExt sgfile ".ygg") ReadMode
     ; sgfcontent <- hGetContents soundgroupfile;
     ; let soundgroups =
            if ".ygg1" `isSubsequenceOf` sgfile
              then
                map ((\(a,b) -> (a,b, Nothing)).read.removeComment)
                    (lines sgfcontent)
              else
                zipWith parseSoundGroup [1..] (lines sgfcontent)

     -- Read the sound change file
     ; soundchangefile <- openFile (fileExt scfile ".ygc") ReadMode
     ; scfcontent <- hGetContents soundchangefile
     ; let soundchanges =
            map (read.removeComment)
                (lines $ subGroups soundgroups scfcontent)

     -- Read the word list file
     ; wordlistinfile <- openFile (fileExt infile ".ygw") ReadMode
     ; wlifcontent <- hGetContents wordlistinfile
     ; let wordlistin = map removeComment $ lines wlifcontent

     -- Write the output file
     ; let wordlistout = applyAllToAll soundchanges soundgroups wordlistin
     ; writeFile (fileExt outfile ".ygw") (unlines wordlistout)
     ; hClose soundchangefile
     ; hClose wordlistinfile
     ; hClose soundgroupfile
     }

-- | Append the given extension to the given filename if no
--   other extension is already present
fileExt :: String -- ^ The filename to append the extension to
        -> String -- ^ The extension to append
        -> String -- ^ The filename with extension
fileExt fn ext = if '.' `elem` fn then fn else fn ++ ext

-- | applies all given <SoundChange>s to all given Strings.
applyAllToAll :: [SoundChange] -- ^ <SoundChange>s to apply
              -> [SoundGroup] -- ^ <SoundGroup>s to use during application
              -> [String] -- ^ Strings to apply SoundChanges to
              -> [String] -- ^ String after application
applyAllToAll scs sgs = map $ foldr ((.).(`applySoundChange` sgs)) id scs

-- | Substitutes long names for short names in the given string
--   for all given SoundGroups, if they have long names
subGroups :: [SoundGroup] -- ^ <SoundGroup>s to use
          -> String -- ^ String to replace in
          -> String -- ^ String after replacement
subGroups = foldr ((.).replaceGroup) id

-- | Substitutes long name for short name in the given string
--   for the given SoundGroup, if it has a long name
replaceGroup :: SoundGroup -- ^ The <SoundGroup> to use
            -> String -- ^ String to replace in
            -> String -- ^ String after replacement
replaceGroup (_, _, Nothing) str = str
replaceGroup (short, _, Just long) str =
  replace ("[" ++ long ++ "]") [short] str
