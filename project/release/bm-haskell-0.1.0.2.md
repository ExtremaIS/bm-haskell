# `bm-haskell` `0.1.0.2` Release Notes

Date
: 2022-03-02

## Overview

`bm` is a utility for opening bookmarks and queries from the command line.
The bookmarks and queries are configured hierarchically in YAML, and they are
referenced using keyword prefixes.  It allows you to quickly open bookmarks
and perform search queries in your browser using only your keyboard.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/bm-haskell#readme>

## This Release

This is a patch release that makes updates to the package infrastructure.  The
package is now published to [Hackage][] and [Stackage][], and some dependency
version upper bounds have been bumped.  There are no changes to the API or
CLI.

[Hackage]: <https://hackage.haskell.org/package/bm>
[Stackage]: <https://stackage.org/package/bm>

### Dependency Versions

The following dependency version upper bounds have been bumped to support the
latest versions.

* [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)
* [`text`](https://hackage.haskell.org/package/text)

### Compatibility

`bm` is currently tested with [GHC 8.6.5][] through [GHC 9.2.1][].  The
`.cabal` file uses Cabal version 1.24 (included with GHC 8.2.2), so it should
build fine on relatively old Haskell installations as well as current
installations.

[GHC 8.6.5]: <https://www.haskell.org/ghc/download_ghc_8_6_5.html>
[GHC 9.2.1]: <https://www.haskell.org/ghc/download_ghc_9_2_1.html>

### Issues

There are no known issues at this time.
