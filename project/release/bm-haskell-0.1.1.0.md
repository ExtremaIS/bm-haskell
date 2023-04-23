# `bm-haskell` `0.1.1.0` Release Notes

Date
: 2023-04-23

## Overview

`bm` is a utility for opening bookmarks and queries from the command line.
The bookmarks and queries are configured hierarchically in YAML, and they are
referenced using keyword prefixes.  It allows you to quickly open bookmarks
and perform search queries in your browser using only your keyboard.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/bm-haskell#readme>

## This Release

`bm` `0.1.0.2` was released more than one year ago, and a number of
updates to dependency constraints have since been registered as
[Hackage revisions][].  This release also updates the package (tarball and
`main` branch) to the latest state.

This release also includes changes to the project management infrastructure.
One important change is that both lower and upper bounds of dependencies are
now tested in CI.

[Hackage revisions]: <https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md#hackage-metadata-revisions--what-they-are-how-they-work>

There are no changes to the API or CLI.

### Compatibility

Build software:

| Software          | `bm` 0.1.0.2      | `bm` 0.1.1.0      |
| ----------------- | ----------------- | ----------------- |
| [GHC][]           | 8.6.5 ~ 9.2.1     | 8.6.5 ~ 9.6.1     |
| [cabal-install][] | 1.24 ~ 3.4        | 1.24 ~ 3.10       |

Library dependencies:

| Package          | `bm` 0.1.0.2      | `bm` 0.1.1.0          |
| ---------------- | ----------------- | --------------------- |
| [aeson][]        | `>=1.4 && < 2.1`  | `>=1.4.6 && < 2.2`    |
| [base][]         | `>=4.7 && <5`     | `>=4.12 && <4.19`     |
| [dlist][]        | `>=0.8 && <1.1`   | `>=0.8.0.4 && <1.1`   |
| [network-uri][]  | `>=2.6 && <2.7`   | `>=2.6.2 && <2.7`     |
| [scientific][]   | `>=0.3 && <0.4`   | `>=0.3.6.2 && <0.4`   |
| [text][]         | `>=1.2.3 && <2.1` | `>=1.2.3.1 && <2.1`   |
| [transformers][] | `>=0.5.6 && <0.6` | `>=0.5.6.2 && <0.7`   |
| [vector][]       | `>=0.12 && <0.13` | `>=0.12.0.1 && <0.14` |

Executable dependencies:

| Package                  | `bm` 0.1.0.2      | `bm` 0.1.1.0        |
| ------------------------ | ----------------- | ------------------- |
| [ansi-wl-pprint][]       | `>=0.6 && <0.7`   | `>=0.6.8 && <0.7`   |
| [directory][]            | `>=1.3 && <1.4`   | `>=1.3.3 && <1.4`   |
| [filepath][]             | `>=1.4 && <1.5`   | `>=1.4.2.1 && <1.5` |
| [optparse-applicative][] | `>=0.14 && <0.18` | `>=0.14 && <0.18`   |
| [typed-process][]        | `>=0.2.6 && <0.3` | `>=0.2.6 && <0.3`   |
| [yaml][]                 | `>=0.11 && <0.12` | `>=0.11.2 && <0.12` |

Test dependencies:

| Package         | `bm` 0.1.0.2      | `bm` 0.1.1.0     |
| --------------- | ----------------- | ---------------- |
| [tasty][]       | `>=1.0 && <1.5`   | `>=1.2 && <1.5`  |
| [tasty-hunit][] | `>=0.10 && <0.11` | `>=0.9 && <0.11` |

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - bm-0.1.1.0
```

[GHC]: <https://www.haskell.org/ghc/>
[cabal-install]: <https://hackage.haskell.org/package/cabal-install>
[aeson]: <https://hackage.haskell.org/package/aeson>
[base]: <https://hackage.haskell.org/package/base>
[dlist]: <https://hackage.haskell.org/package/dlist>
[network-uri]: <https://hackage.haskell.org/package/network-uri>
[scientific]: <https://hackage.haskell.org/package/scientific>
[text]: <https://hackage.haskell.org/package/text>
[transformers]: <https://hackage.haskell.org/package/transformers>
[vector]: <https://hackage.haskell.org/package/vector>
[ansi-wl-pprint]: <https://hackage.haskell.org/package/ansi-wl-pprint>
[directory]: <https://hackage.haskell.org/package/directory>
[filepath]: <https://hackage.haskell.org/package/filepath>
[optparse-applicative]: <https://hackage.haskell.org/package/optparse-applicative>
[typed-process]: <https://hackage.haskell.org/package/typed-process>
[yaml]: <https://hackage.haskell.org/package/yaml>
[tasty]: <https://hackage.haskell.org/package/tasty>
[tasty-hunit]: <https://hackage.haskell.org/package/tasty-hunit>

### Issues

There are no known issues at this time.
