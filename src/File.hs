module File
  ( expandDirectories,
  )
where

import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

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
