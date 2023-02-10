module Kept
  ( KeptOptions (..),
    exportNotes,
    exportNoteToFile,
    exportNoteToStdOut,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime)
import Markdown (MarkdownOpts (..), noteToMarkdownSystemTZ)
import Note (Attachment, Metadata (..), Note (..), getAttachments)
import Parse (parseNote)
import Path (PathOptions (..), getNotePath)
import System.Directory (copyFileWithMetadata, createDirectoryIfMissing, doesFileExist, setModificationTime)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))

-- Prepended to all output file paths, keep everything together.
keptOutputDir :: FilePath
keptOutputDir = "kept-output"

-- Where media attachments go. A subdir of `keptOutputDir`.
attachmentDir :: FilePath
attachmentDir = "media"

data KeptOptions = KeptOptions
  { stdOut :: Bool,
    pathOptions :: PathOptions,
    markdownOptions :: MarkdownOpts
  }

-- Simple container for text that lives at some filepath.
data File = File
  { path :: FilePath,
    content :: T.Text,
    lastModified :: UTCTime
  }

-- Given a set of options and a list of filenames for the JSON input files,
-- produce markdown output. Either to files or printed to stdout.
exportNotes :: KeptOptions -> [FilePath] -> IO ()
exportNotes opts@(KeptOptions stdOut _ _) inFiles = do
  if stdOut
    then mapM_ (printNoteWithPadding opts) inFiles
    else mapM_ (exportNoteToFile opts) inFiles
  putStrLn "Export complete!"
  where
    printNoteWithPadding o f = do
      putStrLn "-----------------------------------------------------------\n"
      exportNoteToStdOut o f
      putStrLn ""

-- Reads the exported Google Keep json from the given file path, converts it to
-- markdown, and writes a new file.
exportNoteToFile :: KeptOptions -> FilePath -> IO ()
exportNoteToFile opts jsonPath = exportNote jsonPath writeNoteFile'
  where
    writeNoteFile' = writeNoteFile opts (takeDirectory jsonPath)

-- Same thing, but just writes path and contents to STDOUT instead of a file.
exportNoteToStdOut :: KeptOptions -> FilePath -> IO ()
exportNoteToStdOut opts jsonPath = exportNote jsonPath printNoteFile'
  where
    printNoteFile' = printNoteFile opts

exportNote :: FilePath -> (Note -> IO ()) -> IO ()
exportNote jsonPath export = do
  json <- TIO.readFile jsonPath
  case parseNote json of
    Left e -> putStrLn $ "Error: " <> e
    Right n -> export n

-- This function does the heavy lifting of rewriting note paths, copying media
-- attachments, and creating markdown files in the correct location.
writeNoteFile :: KeptOptions -> FilePath -> Note -> IO ()
writeNoteFile opts prevAttachmentPath note = do
  let updatedNote = updateNoteAttachments note
  (File fullNotePath content modified) <- noteToFile opts updatedNote
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
  -- ACTUALLY finally, copy media attachments to the correct output dir:
  -- TODO: only create dir if attachments exist?
  _ <- createDirectoryIfMissing createParentDirs (keptOutputDir </> attachmentDir)
  copyAttachmentFiles prevAttachmentPath note

-- Copies attachment files, which initially live in the same directory as their
-- associated JSON note. These paths are slightly different than the relative
-- attachment paths written to the final note. Note that this function
-- preserves attachment file metadata wherever possible.
copyAttachmentFiles :: FilePath -> Note -> IO ()
copyAttachmentFiles prevAttachmentPath note = mapM_ copyAttachment $ getAttachments note
  where
    copyAttachment a =
      copyFileWithMetadata -- this preserves ownership and access times, if possible
        (prevAttachmentPath </> takeFileName a) -- copy FROM
        (outputDir </> takeFileName a) -- copy TO
    outputDir = keptOutputDir </> attachmentDir

-- Updates all of a Note's attachment paths, prepends our output directories.
updateNoteAttachments :: Note -> Note
updateNoteAttachments note = note {metadata = newMetadata}
  where
    newMetadata = (metadata note) {attachments = newAttachments}
    newAttachments = newAttachmentPath $ getAttachments note

-- Prepend the `media` directory to our attachment paths. Note this is the
-- final RELATIVE path that will be written to the markdown note.
newAttachmentPath :: [Attachment] -> [Attachment]
newAttachmentPath = map (mediaDirectory </>)
  where
    mediaDirectory = ".." </> attachmentDir

-- Checks if the given file name already exists. If it does, add a
-- parenthetical number. Keep incrementing that number until the file doesn't
-- exist. Easiest way to guarantee uniqueness.
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

printNoteFile :: KeptOptions -> Note -> IO ()
printNoteFile opts note = do
  (File path content _) <- noteToFile opts note
  putStrLn $ "NOTE PATH:\n" <> path <> "\n\n" <> "NOTE CONTENT:"
  TIO.putStrLn content

noteToFile :: KeptOptions -> Note -> IO File
noteToFile (KeptOptions _ pathOpts mdOpts) n = do
  markdown <- noteToMarkdownSystemTZ mdOpts n
  return
    File
      { path = getNotePath n pathOpts,
        content = markdown,
        lastModified = lastEditedTime (metadata n)
      }
