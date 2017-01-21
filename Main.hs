module Main where

import System.Environment (getArgs)
import System.IO
import Data.Map (fromList, Map)

import SoundChange
import SoundGroup

main :: IO ()
main = do
  args <- getArgs

  soundchangefile <- openFile ((args !! 1) ++ ".hsc") ReadMode
  scfcontent <- hGetContents soundchangefile
  let soundchanges = map read (lines scfcontent)

  wordlistinfile <- openFile (head args ++ ".hin") ReadMode
  wlifcontent <- hGetContents wordlistinfile
  let wordlistin = lines wlifcontent

  soundgroupfile <- openFile ((args !! 2) ++ ".hsg") ReadMode
  sgfcontent <- hGetContents soundgroupfile
  let soundgroups = fromList $ map read (lines sgfcontent)

  let wordlistout = applyAllToAll soundchanges soundgroups wordlistin
  writeFile ((args !! 3) ++ ".hout") (unlines wordlistout)
  hClose soundchangefile
  hClose wordlistinfile
  hClose soundgroupfile


applyAllToAll :: [SoundChange] -> Map Char SoundGroup -> [String] -> [String]
applyAllToAll scs sgs =
  map $ foldr ((.).(`applySoundChange` sgs)) id scs
