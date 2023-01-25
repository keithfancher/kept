module Main (main) where

import Args (KeptOptions (..), cliOptParser)
import Kept (exportNoteToFile, exportNoteToStdOut)
import Options.Applicative (execParser)

main :: IO ()
main = do
  (KeptOptions stdOut pathOpts inFiles) <- execParser cliOptParser
  if stdOut
    then mapM_ (printNoteWithPadding pathOpts) inFiles
    else mapM_ (exportNoteToFile pathOpts) inFiles
  putStrLn "Export complete!"
  where
    printNoteWithPadding pathOpts f = do
      putStrLn "-----------------------------------------------------------\n"
      exportNoteToStdOut pathOpts f
      putStrLn ""
