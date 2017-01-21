module Main where

import System.Environment (getArgs)
import System.IO
import Data.Map (fromList, Map)

import SoundChange
import SoundGroup

main :: IO ()
main = do
  args <- getArgs

  soundchangefile <- openFile (args !! 1) ReadMode
  scfcontent <- hGetContents soundchangefile
  let soundchanges = map read (lines scfcontent)
  hClose soundchangefile

  wordlistinfile <- openFile (head args) ReadMode
  wlifcontent <- hGetContents wordlistinfile
  let wordlistin = lines wlifcontent
  hClose wordlistinfile

  soundgroupfile <- openFile (args !! 2) ReadMode
  sgfcontent <- hGetContents soundgroupfile
  let soundgroups = fromList $ map read (lines sgfcontent)
  hClose soundgroupfile

  let wordlistout = applyAllToAll soundchanges soundgroups wordlistin
  writeFile (args !! 3) (unlines wordlistout)


applyAllToAll :: [SoundChange] -> Map Char SoundGroup -> [String] -> [String]
applyAllToAll scs sgs =
  map $ foldl (.) id $ map (`applySoundChange` sgs) scs
