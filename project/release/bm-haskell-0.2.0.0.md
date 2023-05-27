# `bm-haskell` `0.2.0.0` Release Notes

Date
: 2023-05-28

## Overview

`bm` is a utility for opening bookmarks and queries from the command line.
The bookmarks and queries are configured hierarchically in YAML, and they are
referenced using keyword prefixes.  It allows you to quickly open bookmarks
and perform search queries in your browser using only your keyboard.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/bm-haskell#readme>

## This Release

This release adds compatibility with the latest version of the
`optparse-applicative` library.

There are no changes to the API or CLI.

### Compatibility

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - bm-0.2.0.0
```

### Issues

There are no known issues at this time.
