module Main (main) where

import Kept (exportNoteToStdOut)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args -- TODO: temporary, of course, for some quick testing
  exportNoteToStdOut filePath
