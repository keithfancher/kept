module Main (main) where

import Args (KeptOptions (..), cliOptParser)
import Kept (exportNoteToFile, exportNoteToStdOut)
import Options.Applicative (execParser)

main :: IO ()
main = do
  (KeptOptions stdIO inFiles) <- execParser cliOptParser
  if stdIO
    then mapM_ printNoteWithPadding inFiles
    else mapM_ exportNoteToFile inFiles
  putStrLn "Export complete!"
  where
    printNoteWithPadding f = do
      putStrLn "-----------------------------------------------------------\n"
      exportNoteToStdOut f
      putStrLn ""
