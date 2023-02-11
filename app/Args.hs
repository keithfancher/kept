module Args
  ( CLIOpts (..),
    cliOptParser,
  )
where

import Kept (KeptOptions (..))
import Markdown (MarkdownOpts (..))
import Options.Applicative
import Path (PathOptions (..))

data CLIOpts = CLIOpts
  { keptOptions :: KeptOptions,
    inFiles :: [FilePath]
  }

cliOptParser :: ParserInfo CLIOpts
cliOptParser =
  info
    (optWrapperParser <**> helper)
    ( fullDesc
        <> header "kept: Extract your data from Google Keep."
        <> progDesc "Pass in one or more of your exported Google Keep JSON files and get some nice markdown in return. Export your Keep JSON data via takeout.google.com."
    )

optWrapperParser :: Parser CLIOpts
optWrapperParser =
  CLIOpts
    <$> keptOptionParser
    <*> filePathsParser

keptOptionParser :: Parser KeptOptions
keptOptionParser =
  KeptOptions
    <$> stdOutFlagParser
    <*> tagPathFlagParser
    <*> markdownOptionsParser

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

markdownOptionsParser :: Parser MarkdownOpts
markdownOptionsParser = markdownOptsFromFlags <$> noYamlFlagParser <*> titleFlagParser

-- Chews up the boolean CLI flags and spits out a proper `MarkdownOpts` object.
markdownOptsFromFlags :: Bool -> Bool -> MarkdownOpts
markdownOptsFromFlags noYaml titleInYaml = case (noYaml, titleInYaml) of
  (True, _) -> NoFrontMatter -- check this option first, it takes precedence
  (False, True) -> FrontMatterWithTitle -- yes front matter, yes include title in front matter
  _ -> FrontMatterDefault -- any remaining option results in default front matter

noYamlFlagParser :: Parser Bool
noYamlFlagParser =
  switch
    ( long "no-yaml"
        <> short 'y'
        <> help "Do not add YAML front-matter to exported notes (takes precedence over `--title-in-yaml` option)"
    )

titleFlagParser :: Parser Bool
titleFlagParser =
  switch
    ( long "title-in-yaml"
        <> short 't'
        <> help "Include `title` field in YAML front-matter INSTEAD OF as a heading in the note body"
    )

filePathsParser :: Parser [FilePath]
filePathsParser =
  some -- `some` == "one or more" (as opposed to `many`, zero or more)
    ( argument
        str
        ( metavar "JSON_FILE(S)"
            <> help "One or more exported Google Keep JSON files"
        )
    )
