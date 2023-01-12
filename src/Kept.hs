module Kept
  ( exportNoteToFile,
    exportNoteToStdOut,
  )
where

import Data.Text.IO qualified as TIO
import File (File (..), noteToFile)
import Parse (KeepJSON, parseNote)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

type Error = String -- TODO: real error type

-- Prepended to all output file paths, keep everything together.
keptOutputDir :: FilePath
keptOutputDir = "kept-output"

-- Reads the exported Google Keep json from the given file path, converts it to
-- markdown, and writes a new file.
exportNoteToFile :: FilePath -> IO ()
exportNoteToFile jsonPath = exportNote jsonPath writeNoteFile

-- Same thing, but just writes path and contents to STDOUT instead of a file.
exportNoteToStdOut :: FilePath -> IO ()
exportNoteToStdOut jsonPath = exportNote jsonPath printNoteFile

writeNoteFile :: File -> IO ()
writeNoteFile (File fullNotePath content) = do
  let noteDirectory = keptOutputDir </> takeDirectory fullNotePath
  let createParentDirs = True
  _ <- createDirectoryIfMissing createParentDirs noteDirectory
  TIO.writeFile (keptOutputDir </> fullNotePath) content

printNoteFile :: File -> IO ()
printNoteFile (File path content) = do
  putStrLn $ "NOTE PATH:\n" <> path <> "\n\n" <> "NOTE CONTENT:"
  TIO.putStrLn content

exportNote :: FilePath -> (File -> IO ()) -> IO ()
exportNote jsonPath export = do
  json <- TIO.readFile jsonPath
  case convertKeepNote json of
    Left e -> putStrLn $ "Error: " <> e
    Right f -> export f

convertKeepNote :: KeepJSON -> Either Error File
convertKeepNote json = noteToFile <$> parseNote json
