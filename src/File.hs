module File
  ( expandDirectories,
    getUniqueFileName,
  )
where

import Data.Text qualified as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, (</>))

-- If any in a list of FilePaths is a directory, replace that directory with a
-- list of all JSON files in that directory. Any non-directory FilePaths are
-- just left alone.
expandDirectories :: [FilePath] -> IO [FilePath]
expandDirectories paths = do
  nestedLists <- mapM expandIfDir paths
  return $ concat nestedLists

expandIfDir :: FilePath -> IO [FilePath]
expandIfDir f = do
  isDir <- doesDirectoryExist f
  if isDir
    then listJSON f
    else return [f]

-- Given a directory, get list of all JSON files in that directory.
listJSON :: FilePath -> IO [FilePath]
listJSON path = do
  files <- listDirectory path
  let json = filter isJSON files
  -- We need to re-add the directory back to each file to make it a valid path:
  return $ map (path </>) json

-- Case-insensitive check on the file extension.
isJSON :: FilePath -> Bool
isJSON p = takeExtension (lower p) == ".json"
  where
    lower = T.unpack . T.toLower . T.pack -- Looks silly, but Text handles unicode sanely so is preferred

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
