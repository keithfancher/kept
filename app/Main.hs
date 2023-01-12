module Main (main) where

import Args (KeptOptions (..), cliOptParser)
import Kept (exportNoteToStdOut)
import Options.Applicative (execParser)

main :: IO ()
main = do
  (KeptOptions stdIO inFiles) <- execParser cliOptParser
  if stdIO
    then mapM_ exportNoteToStdOut inFiles
    else error "implement me! ;)"
