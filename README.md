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
  each note's [YAML front
  matter](https://help.obsidian.md/Advanced+topics/YAML+front+matter), and so
  are its tags
- Filesystem metadata is also updated, i.e. the `lastEditedTime` for each note
  will be reflected in the created file's "last modified time"
- Exported notes are sorted into subdirectories based on label, archive/trash
  status, and "pinned" status
- Untitled notes are given filenames generated from note content, rather than
  Google's "timestamp-as-title" approach
- Full support for media attachments (audio/images)


## Quick-start

`kept` uses JSON data exported from [Google
Takeout](https://takeout.google.com/). The first step is to export your data.

The quickest approach is to log into Google Takeout, "de-select all", then
select *only* Keep for your export. (If you export everything, chances are it
will take a very long time and produce a huge amount of data.)

Once you've got a directory full of exported Google Keep JSON, you can convert
*any* or *all* of these files using `kept`. [Grab the latest
release](https://github.com/keithfancher/kept/releases) for your platform of
choice, and take it for a test-drive.

Here are some simple examples:

```
# This will convert a single note. By default, the markdown output will be
# written to a file in the `kept-output` directory, relative to your current
# working directory. More details below re: the full path of this file and
# its name.
$ kept ~/Takeout/Keep/Todo.json

# Alternatively, you can convert everything at once! (Be sure to actually glob
# for `*.json`, as there will also likely be HTML and some other random files
# in your Google Takeout export.)
$ kept ~/Takeout/Keep/*.json

# Use the `-s` (or `--stdout`) option to print the converted markdown content
# to stdout rather than writing to a file. Useful to preview how a note would
# look, if you're curious:
$ kept -s "~/Takeout/Keep/Great Ideas.json"
```


## Installation

You've got two options:

1. Build from source. Not recommended unless you need a particular
   cutting-edge feature that isn't included in the latest binary release. (But
   don't worry: building isn't *complicated*. It's just *slow*.)
2. Install a binary [release](https://github.com/keithfancher/kept/releases).
   The quick-and-easy route.

More details below.

### Building from source

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
your system's equivalent). You may need to add this directory to your `PATH`.

Alternatively, you can simply put the binary wherever you'd like! There are no
ancillary files to install.

Yet another option is just to execute `kept` from within the project
directory, using `stack`:

```
# Note the extra `--` required here, when using `stack exec`:
$ stack exec kept -- -s ~/Takeout/Keep/test.json
```

To uninstall, simply remove the `kept` binary from wherever you installed it.

### Binary installation

Download the latest build for your platform of choice from the Github
[releases page](https://github.com/keithfancher/kept/releases). Extract the
`kept` binary and put it somewhere in your `PATH`. That's it!

And again, to uninstall: simply remove the `kept` binary from wherever you
installed it.


## Paths of exported notes

All output goes into a `kept-output` directory, relative to your current
working directory. `kept` will further sort your notes into subdirectories
based on certain criteria:

1. If a note is in Keep's `Trash`, `Archive`, or is `Pinned`, it will go into
   the `trash`, `archive`, or `pinned` subdirectory as appropriate. Those
   statuses are checked *in that order*. (In other words, if a note is
   "pinned" and you move it to the trash in Keep, then export your notes,
   `kept` will put that note in the `trash` subdirectory.)
2. By default, any note which isn't trashed, archived, or pinned is put into a
   subdirectory named for that note's label/tag.

Tags-as-subdirectories works great if you used Keep labels like folders, but
gets a little messier for notes with multiple labels. In that case, `kept`
will sort the labels and connect them with dashes to determine the
subdirectory. Notes with the same *set* of labels will go in the same
subdirectory.

(For example, if a note has the `Important` and `Dumb` labels, we'll put it
into the `Dumb-Important` subdirectory.)

Note that you can disable this behavior with the `--no-tag-subdirs` CLI
option. See "Options" below for more detail.


## Note contents / Front-matter

Google Keep notes don't contain formatting, so each note's contents will be
exported more-or-less as-is. If a note has a title, that title will be written
as a top-level markdown heading. (The title is also used as a filename for the
note.) Checklists are converted into standard markdown checklists.

By default, each note's metadata will be included as YAML front-matter at the
top of the file. (Many common editors support this, including Obsidian and
Markor.) The included front-matter fields are: `tags`, `createdTime`, and
`lastEditedTime`.

You can disable the YAML front-matter entirely with the `--no-yaml` CLI
option. (See the "Options" section below.) Note that if you do, it might make
it harder for your editor-of-choice to determine the tag(s) of your notes. (If
you've let `kept` sort them into subdirectories, that might not matter.)


## Filesystem metadata

`kept` will also update each created file's "last modified date", assuming
your filesystem supports this. (Most do.) This means your normal file-explorer
tools -- and tools like Obsidian -- can sort by recently-modified, find notes
from a specific date range, etc.

Note that this is *totally independent* from the metadata stored in a note's
YAML front-matter. In other words, you can disable the YAML front-matter and
still be confident that your notes will be date-and-time sortable, searchable,
and so on.


## Media attachments

If a Keep note has media attachments (audio or images), `kept` will do a few
things:

1. Copy those files to a `media` subdirectory within `kept-output`. (Media
   file metadata will be preserved wherever possible.)
2. Include a list of those media attachments in the YAML front-matter of the
   note.
3. Embed the media directly in the note body, using standard
   (Obsidian-friendly) markdown syntax.

This yields a final output that looks quite similar to the way media
attachments are presented in the original Google Keep note.

Note that `kept` will *not* rename your media files as it copies them over, so
whatever funky names Google Takeout has given them will remain. If you'd like
to rename them, I suggest using Obsidian to do so, since it keeps track of
file backlinks and can update linked files accordingly.


## Options / Flags

- `--stdout` / `-s`: Print output to screen rather than writing files. Useful
  to preview the results of an export.
- `--no-tag-subdirs` / `-t`: Do not sort notes into tag-based subdirectories.
  Notes will still be sorted into `trash`, `archive`, and `pinned`, but all
  *remaining* notes will go into a single `all-notes` subdirectory.
- `--no-yaml` / `-y`: Do *not* add YAML front-matter to exported notes. By
  default, each note will have `tags`, `createdTime`, and `lastEditedTime`
  properties included in its YAML front-matter. This option leaves out the
  front-matter entirely.
