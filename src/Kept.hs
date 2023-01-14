module Kept
  ( exportNoteToFile,
    exportNoteToStdOut,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (TimeZone, UTCTime, getCurrentTimeZone)
import Markdown (noteToMarkdown)
import Note (Metadata (..), Note (..))
import Parse (KeepJSON, ParseError, parseNote)
import Path (getNotePath)
import System.Directory (createDirectoryIfMissing, doesFileExist, setModificationTime)
import System.FilePath (dropExtension, takeDirectory, takeExtension, (</>))

-- Simple container for text that lives at some filepath.
data File = File
  { path :: FilePath,
    content :: T.Text,
    lastModified :: UTCTime
  }
  deriving (Show, Eq)

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
writeNoteFile (File fullNotePath content modified) = do
  -- First create the directory for our note:
  let noteDirectory = keptOutputDir </> takeDirectory fullNotePath
  let createParentDirs = True
  _ <- createDirectoryIfMissing createParentDirs noteDirectory
  -- Make sure we have a unique file name and write the file:
  uniqueNotePath <- getUniqueFileName (keptOutputDir </> fullNotePath)
  putStrLn $ "Writing file to " <> uniqueNotePath
  TIO.writeFile uniqueNotePath content
  -- And finally, set the modification timestamp to match the note metadata:
  setModificationTime uniqueNotePath modified

-- Checks if the given file name already exists. If it does, add a
-- parenthetical number. Keep incrementing that number until the file doesn't
-- exist. (Could also use metadata timestamp? But even that's not guaranteed to
-- be unique.)
getUniqueFileName :: FilePath -> IO FilePath
getUniqueFileName f = go f 0
  where
    go file attemptNum = do
      let fn = addNumberToFileName file attemptNum
      fileAlreadyExists <- doesFileExist fn
      if fileAlreadyExists
        then go file (attemptNum + 1)
        else return fn

addNumberToFileName :: FilePath -> Int -> FilePath
addNumberToFileName f 0 = f
addNumberToFileName f n = sansExtension <> " (" <> show n <> ")" <> extension
  where
    sansExtension = dropExtension f
    extension = takeExtension f

printNoteFile :: File -> IO ()
printNoteFile (File path content _) = do
  putStrLn $ "NOTE PATH:\n" <> path <> "\n\n" <> "NOTE CONTENT:"
  TIO.putStrLn content

exportNote :: FilePath -> (File -> IO ()) -> IO ()
exportNote jsonPath export = do
  json <- TIO.readFile jsonPath
  -- TODO: subtle bug here -- will apply the CURRENT daylight savings state to
  -- the timestamp, rather than what it would have been at that time. Maybe do
  -- something with `utcToLocalZonedTime`?
  tz <- getCurrentTimeZone
  case convertKeepNote json tz of
    Left e -> putStrLn $ "Error: " <> e
    Right f -> export f

convertKeepNote :: KeepJSON -> TimeZone -> Either ParseError File
convertKeepNote json tz = noteToFile <$> parseNote json
  where
    noteToFile n =
      File
        { path = getNotePath n,
          content = noteToMarkdown n tz,
          lastModified = lastEditedTime (metadata n)
        }
