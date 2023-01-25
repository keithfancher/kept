module Args
  ( KeptOptions (..),
    cliOptParser,
  )
where

import Options.Applicative
import Path (PathOptions (..))

data KeptOptions = KeptOptions
  { stdOut :: Bool,
    pathOptions :: PathOptions,
    inFiles :: [FilePath]
  }

cliOptParser :: ParserInfo KeptOptions
cliOptParser =
  info
    (keptOptionParser <**> helper)
    ( fullDesc
        <> header "kept: Extract your data from Google Keep."
        <> progDesc "Pass in one or more of your exported Google Keep JSON files and get some nice markdown in return. Export your Keep JSON data via takeout.google.com."
    )

keptOptionParser :: Parser KeptOptions
keptOptionParser =
  KeptOptions
    <$> stdOutFlagParser
    <*> tagPathFlagParser
    <*> filePathsParser

stdOutFlagParser :: Parser Bool
stdOutFlagParser =
  switch
    ( long "stdout"
        <> short 's'
        <> help "Print output to screen rather than writing files"
    )

tagPathFlagParser :: Parser PathOptions
tagPathFlagParser =
  toPathOpt
    <$> switch
      ( long "no-tag-subdirs"
          <> short 'n'
          <> help "Do not sort notes into tag-based subdirectories"
      )
  where
    -- This is a bit confusing because of the negation. These `switch` options
    -- are False by default, so that needs to map to our default desired behavior:
    toPathOpt False = TagSubDirs
    toPathOpt True = NoTagSubDirs

-- We get these in from the CLI as a list of paths, but need to transform it
-- into `InFiles`, the actually-useful type.
filePathsParser :: Parser [FilePath]
filePathsParser =
  some -- `some` == "one or more" (as opposed to `many`, zero or more)
    ( argument
        str
        ( metavar "JSON_FILE(S)"
            <> help "One or more exported Google Keep JSON files"
        )
    )
