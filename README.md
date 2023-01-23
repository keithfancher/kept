# kept

A work-in-progress tool to convert exported [Google
Keep](https://keep.google.com/) data into plain markdown notes (for use with
[Obsidian](https://obsidian.md/),
[Markor](https://github.com/gsantner/markor), or [any other text
editor](https://neovim.io/)).

(It's "keep"... in the past tense. Get it?)


## Features

- Converts Keep "labels" into tags that are [fully compatible with
  Obsidian](https://help.obsidian.md/How+to/Working+with+tags#Allowed+characters)
- Preserves note metadata: `createdTime` and `lastEditedTime` are included in
  the notes' [YAML front
  matter](https://help.obsidian.md/Advanced+topics/YAML+front+matter), as well
  as its tags
- Filesystem metadata is also updated, i.e. the `lastEditedTime` for each note
  will be reflected in the created files' "last modified time"
- Exported notes are sorted into subdirectories based on label, archive/trash
  status, and "pinned" status
- Untitled notes are given file names generated from note content


## Quick-start

`kept` uses JSON data exported from [Google
Takeout](https://takeout.google.com/). The first step is to export your data.

The quickest approach is log in to Google Takeout, "de-select all", then
select *only* Keep for your export. If you export everything, chances are it
will take a very long time and produce a huge amount of data.

Once you've got a directory full of exported Google Keep JSON, you can convert
*any* or *all* of these files using `kept`. Here are some simple examples:

```
# This will convert a single note. By default, the markdown output will be
# written to a file in the `kept-output` directory, relative to your current
# working directory. More details below re: the  full path of this file and
# its name.
$ kept ~/Takeout/Keep/Todo.json

# Alternatively, you can convert everything at once! (Be sure to actually glob
# for `*.json` or something similar, as there will also likely be HTML and some
# other random files in your Google Takeout export.)
$ kept ~/Takeout/Keep/*.json

# Use the `-s` (or `--stdio`) option to print the converted markdown content
# to stdout rather than writing to a file. Useful to preview how a note would
# look, if you're curious:
$ kept -s "~/Takeout/Keep/Great Ideas.json"
```


## Building from source

We use Stack as our build system. You can [install it
directly](https://docs.haskellstack.org/en/stable/#how-to-install-stack) or
via [GHCup](https://www.haskell.org/ghcup/).

Once Stack is installed, navigate to the `kept` project root (where the
`package.yaml` file is located) and `stack build`:

```
$ cd kept
$ stack build
$ stack install
```

The `stack install` command will put the created binary in `~/.local/bin` (or
your system's equivalent). You may need to add this directory to your `$PATH`.

Alternatively, you can simply put the binary wherever you'd like! There are no
ancillary files to install.

Yet another option is just to execute `kept` from within the project
directory, using `stack`:

```
# Note the extra `--` required here, when using `stack exec`:
$ stack exec kept -- -s ~/Takeout/Keep/test.json
```

To uninstall, simply remove the `kept` binary from wherever you installed it.


## Binary installation

I haven't built any binary releases yet. Coming soon!


## More details?

...are coming soon!
