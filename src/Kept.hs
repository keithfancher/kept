module Kept
  ( exportNoteToFile,
    exportNoteToStdOut,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime)
import Markdown (MarkdownOpts (..), noteToMarkdownSystemTZ)
import Note (Metadata (..), Note (..))
import Parse (KeepJSON, ParseError, parseNote)
import Path (PathOptions (..), getNotePath)
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
exportNoteToFile :: PathOptions -> FilePath -> IO ()
exportNoteToFile pathOpts jsonPath = exportNote pathOpts jsonPath writeNoteFile

-- Same thing, but just writes path and contents to STDOUT instead of a file.
exportNoteToStdOut :: PathOptions -> FilePath -> IO ()
exportNoteToStdOut pathOpts jsonPath = exportNote pathOpts jsonPath printNoteFile

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

exportNote :: PathOptions -> FilePath -> (File -> IO ()) -> IO ()
exportNote pathOpts jsonPath export = do
  json <- TIO.readFile jsonPath
  noteFile <- convertKeepNote pathOpts json
  case noteFile of
    Left e -> putStrLn $ "Error: " <> e
    Right f -> export f

convertKeepNote :: PathOptions -> KeepJSON -> IO (Either ParseError File)
convertKeepNote pathOpts json = mapM (noteToFile pathOpts) (parseNote json)

noteToFile :: PathOptions -> Note -> IO File
noteToFile pathOpts n = do
  markdown <- noteToMarkdownSystemTZ NoFrontMatter n
  return
    File
      { path = getNotePath n pathOpts,
        content = markdown,
        lastModified = lastEditedTime (metadata n)
      }
