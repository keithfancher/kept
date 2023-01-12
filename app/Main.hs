module Main (main) where

import Args (KeptOptions (..), cliOptParser)
import Kept (exportNoteToStdOut)
import Options.Applicative (execParser)

main :: IO ()
main = do
  (KeptOptions stdIO inFiles) <- execParser cliOptParser
  let filePath = head inFiles -- TODO: temporary, of course, for some quick testing
  if stdIO
    then exportNoteToStdOut filePath
    else error "implement me! ;)"
