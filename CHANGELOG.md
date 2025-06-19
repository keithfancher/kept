# Changelog for `kept`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).


## Unreleased

### Changed

- License update: `BSD-3-Clause` to `GPL-3.0-or-later`. Maybe this should go
  in the "Fixed" section ;)


## 0.3.0 - 2023-02-17

### Added

- Allow passing directories of JSON to `kept` rather than having to manually
  glob, e.g. with `*.json`. Now one can pass in any combination of individual
  files, directories, or globbed lists of files. If a directory is passed in,
  it is expanded (non-recursively) into the list of the `.json` files in that
  directory.

### Fixed

- Windows: embedded media attachments display correctly now. (Turns out
  Obsidian wants `/` as a path delimiter even in Windows.)


## 0.2.0 - 2023-02-13

### Added

- Full support for media attachments!
- New CLI option to include a note's title as a field in the YAML front-matter
  *instead of* as a heading in the note's body.

### Changed

- Short CLI option for `--no-tag-subdirs` is now `-n` rather than `-t`. Frees
  `-t` up for our new title option!


## 0.1.0 - 2023-01-30

### Added

- Everything! Almost. Convert Keep JSON to markdown, but no media attachments
  yet.
