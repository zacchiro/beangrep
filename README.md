![CI status](https://github.com/zacchiro/beangrep/actions/workflows/python-package.yml/badge.svg)


Beangrep - grep-like filter for Beancount
=========================================

[Beangrep][beangrep-home] is a grep-like filter for the [Beancount][beancount-home] plain text accounting system.

[beangrep-home]: https://github.com/zacchiro/beangrep
[beancount-home]: http://beancount.github.io/


Installation
------------

### Installing from source

```console
$ git clone https://github.com/zacchiro/beangrep
$ cd beangrep

$ python3 -m venv ./venv    # optional but recommended
$ source venv/bin/activate
(venv) $ pip install -e .
...
Successfully installed beancount-2.3.6 beangrep-...

$ bean-grep --help
Usage: bean-grep [OPTIONS] FILENAME
[...]
```


Usage
-----

The CLI says it all when invoked as `bean-grep --help`:

```
Usage: bean-grep [OPTIONS] FILENAME

  Search for entries matching given criteria in a Beancount ledger. Pretty
  print matching entries to standard output.

  The granularity of matching (and results) is that of individual entries,
  e.g., full transactions, balances, notes, etc. By default only transactions
  are returned; use the --type/-T option to override.

  To read from standard input, pass "-" as FILENAME, but beware that it
  implies on-disk buffering of stdin.

Options:
  -a, --account REGEX             Only return entries referencing accounts
                                  with names matching given regex.
  -A, --amount "[OP]AMOUNT [REGEX]"
                                  Only return entries with amounts matching
                                  the given amount predicate. An amount
                                  predicate start with an optional comparison
                                  operator (one of '<', '<=', '=', '>=', '>',
                                  with '=' being the default), followed by a
                                  decimal number (using '.' as decimal
                                  separator), followed by an optional currency
                                  regex. Multiple amount predicates can be
                                  given to express complex amount ranges.
  -d, --date [OP]DATE             Only return entries with dates matching the
                                  given date predicate. A date predicate start
                                  with an optional comparison operator (one of
                                  '<', '<=', '=', '>=', '>', with '=' being
                                  the default), and is followed by a date in
                                  the form YYYY-[MM[-DD]]. Multiple date
                                  predicates can be given to express complex
                                  date ranges.
  -m, --meta, --metadata REGEX[:REGEX]
                                  Only return entries with at least one
                                  metadata key/value pair matching given
                                  pattern. A pattern is a pair of regexs
                                  separated by ':', the former matching on
                                  metadata key, the latter on metadata value.
                                  The second regex is optional and defaults to
                                  '.*'.
  -n, --narration REGEX           Only return entries whose narrations match
                                  given regex.
  -p, --payee REGEX               Only return entries whose payees match given
                                  regex.
  -s, --somewhere, --anywhere REGEX
                                  Only return entries with a value in them,
                                  anywhere, matching given regex.
  -t, --tag REGEX                 Only return entries with at least one tag
                                  matching given regex. The tag can be located
                                  anywhere.
  -T, --type TYPE(S)              Only return entries of certain types. Types
                                  are specified as a '|'-separated list of
                                  type names; type names are: open, close,
                                  commodity, pad, balance, transaction, note,
                                  event, query, price, document, custom. The
                                  special value 'all' means: all directive
                                  types.  [default: transaction]
  -i, --ignore-case / --no-ignore-case
                                  Ignore case distinctions in string matches.
                                  [default: no-ignore-case]
  --posting-tags-meta TEXT        Metadata key used to attach tags to
                                  transaction postings.  [default: tags]
  -q, --quiet / --no-quiet        Quiet, do not write anything to standard
                                  output. Exit succesfully immediately if any
                                  match is found.  [default: no-quiet]
  --skip-internals / --no-skip-internals
                                  When matching, ignore internal information
                                  not visible in the ledger. This includes the
                                  automatic metadata: {'filename', 'lineno'}
                                  [default: skip-internals]
  -v, --verbose                   Increase logging verbosity. Default level is
                                  WARNING. Passing this option once (e.g., -v)
                                  will increase it to INFO, twice or more
                                  (e.g., -vv) to DEBUG.
  --help                          Show this message and exit.
```


Author
------

* [Stefano Zacchiroli][zack-home] [`<zack@upsilon.cc>`][zack-email]

[zack-home]: https://upsilon.cc/~zack
[zack-email]: mailto:zack@upsilon.cc


License
-------

Beangrep is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

A copy of the GNU General Public License is [distributed with Beangrep][gpl2-here]
as well as from the [official license page][gpl2-home].

[gpl2-here]: ./LICENSE-GPL-2.0-or-later
[gpl2-home]: https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
