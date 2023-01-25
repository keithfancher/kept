module Args
  ( KeptOptions (..),
    cliOptParser,
  )
where

import Options.Applicative

data KeptOptions = KeptOptions
  { stdOut :: Bool,
    inFiles :: [FilePath]
  }

cliOptParser :: ParserInfo KeptOptions
cliOptParser =
  info
    (keptOptionParser <**> helper)
    ( fullDesc
        <> header "kept: Extract your data from Google Keep."
        <> progDesc "TODO: do a thing, give it your JSON files, etc" -- TODO
    )

keptOptionParser :: Parser KeptOptions
keptOptionParser =
  KeptOptions
    <$> stdOutFlagParser
    <*> filePathsParser

stdOutFlagParser :: Parser Bool
stdOutFlagParser =
  switch
    ( long "stdout"
        <> short 's'
        <> help "..." -- TODO
    )

-- We get these in from the CLI as a list of paths, but need to transform it
-- into `InFiles`, the actually-useful type.
filePathsParser :: Parser [FilePath]
filePathsParser =
  some -- `some` == "one or more" (as opposed to `many`, zero or more)
    ( argument
        str
        ( metavar "JSON_FILE(S)"
            <> help "..." -- TODO
        )
    )
