---
title: BM
section: 1
hyphenate: false
...

# NAME

`bm` - open bookmarks and queries from the command line

# SYNOPSIS

`bm` [*OPTIONS*] [ARG ...]

# DESCRIPTION

`bm` is a utility for opening bookmarks and queries from the command line.

# OPTIONS

-h, \--help
:   show help and exit

\--version
:   show version and exit

-c, \--config *CONFIG*
:   config file (default: `$XDG_CONFIG_HOME/bm.yaml`)

-t, \--trace
:   show trace output for debugging

# ARGUMENTS

*ARG*
:   keyword (prefix) to traverse the bookmark hierarchy

# EXIT CODES

0
:   no error

1
:   program error

2
:   usage error

# CONFIGURATION

Bookmarks and queries are configured in a YAML file.  The configuration file
`$XDG_CONFIG_HOME/bm.yaml` is used by default.  On non-Windows systems, this
is usually `~/.config/bm.yaml`.

The command used to open bookmarks and queries defaults to the following
commands, depending on the operating system:

* Linux: `xdg-open`
* Windows: `start`
* macOS: `open`

The configuration file has the following properties:

*command*
:   command used to open bookmarks and queries (string, optional)

*args*
:   list of bookmarks (array of bookmark objects)

If you specify a `command` here, it takes precedence over the default
described above.

Bookmark objects have the following properties:

*keyword*
:   keyword to match against command-line arguments (string)

*command*
:   command used to open bookmarks and queries (string, optional)

*url*
:   bookmark URL (string, optional)

*query*
:   query definition (query object, optional)

*args*
:   list of child bookmarks (array of bookmark objects)

Any scalar value can be used to define a `keyword`.  If you specify a
`command` for a bookmark, that command takes precedence for that bookmark as
well as any child bookmarks.  Only one of `query` and `args` may be specified.

Query objects have the following properties:

*action*
:   query action URL (string)

*parameter*
:   query parameter name (string, optional, default: "q")

*hidden*
:   list of hidden parameters (array of parameter object, optional)

Parameter objects have the following properties:

*name*
:   parameter name (string)

*value*
:   parameter value (string)

Any scalar value can be used to define a `value`.

# TRACING

The `--trace` option shows trace output for debugging.  Commands are displayed
in brackets such as `[xdg-open]`.  Bookmark keywords are displayed in angle
brackets such as `<keyword>`.  When there is no error, the command that is
executed to open the bookmark/query is displayed without escaping.

# PROJECT

GitHub:
:   <https://github.com/ExtremaIS/bm-haskell>

Reporting issues:
:   GitHub: <https://github.com/ExtremaIS/bm-haskell/issues>

    Email: <bugs@extrema.is>

Copyright
:   Copyright (c) 2021 Travis Cardwell

License
:   The MIT License <https://opensource.org/licenses/MIT>
