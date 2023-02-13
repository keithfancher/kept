# Changelog for `kept`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).


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
