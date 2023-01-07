module Main (main) where

import Data.Text.IO qualified as TIO
import Markdown (noteToMarkdown)
import Parse (parseNote)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  json <- TIO.readFile $ head args -- TODO: temporary, of course, for some quick testing
  case parseNote json of
    Nothing -> TIO.putStrLn "Parse error :'("
    Just note -> TIO.putStrLn $ noteToMarkdown note
