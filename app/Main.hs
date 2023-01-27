module Main (main) where

import Args (CLIOpts (..), cliOptParser)
import Kept (exportNotes)
import Options.Applicative (execParser)

main :: IO ()
main = do
  (CLIOpts keptOpts inFiles) <- execParser cliOptParser
  exportNotes keptOpts inFiles
