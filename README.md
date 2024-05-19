![CI status](https://github.com/zacchiro/beangrep/actions/workflows/python-package.yml/badge.svg)


Beangrep - grep-like filter for Beancount
=========================================

[Beangrep][beangrep-home] is a grep-like filter for the [Beancount][beancount-home] plain text accounting system.

[beangrep-home]: https://github.com/zacchiro/beangrep
[beancount-home]: http://beancount.github.io/


Installation
------------

### Prebuilt package

```console
$ python3 -m venv ./venv    # optional but recommended
$ source venv/bin/activate

$ pip install beangrep
[...]
Successfully installed beancount-2.3.6 beangrep-...

$ bean-grep --help
Usage: bean-grep [OPTIONS] FILENAME
[...]
```

### From source

```console
$ git clone https://github.com/zacchiro/beangrep
$ cd beangrep

$ python3 -m venv ./venv    # optional but recommended
$ source venv/bin/activate

$ pip install -e .
[...]
Successfully installed beancount-2.3.6 beangrep-...

$ bean-grep --help
Usage: bean-grep [OPTIONS] FILENAME
[...]
```


Usage
-----

The CLI says it all when invoked as `bean-grep --help`:

```
Usage: bean-grep [OPTIONS] [PATTERN] FILENAME...

  Search for entries matching given criteria in Beancount journals. Pretty
  print matching entries to standard output.

  Search criteria can be specified with the options below and/or providing an
  explicit "smart" PATTERN. If given, PATTERN is interpreted as described
  below under "Patterns". If not given, search criteria are defined by
  explicit options.

  Multiple options, options given mutiple times, and PATTERN(s) are logically
  joined (AND-ed) together.

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
                                  operator (one of "<", "<=", "=", ">=", ">",
                                  with "=" being the default), followed by a
                                  decimal number (using "." as decimal
                                  separator), followed by an optional currency
                                  regex. Multiple amount predicates can be
                                  given to express complex amount ranges.
  -d, --date [OP]DATE             Only return entries with dates matching the
                                  given date predicate. A date predicate start
                                  with an optional comparison operator (one of
                                  "<", "<=", "=", ">=", ">", with "=" being
                                  the default), and is followed by a date in
                                  the form YYYY-[MM[-DD]]. Multiple date
                                  predicates can be given to express complex
                                  date ranges.
  -l, --link REGEX                Only return entries with at least one link
                                  matching given regex.
  -m, --meta, --metadata REGEX[:REGEX]
                                  Only return entries with at least one
                                  metadata key/value pair matching given
                                  pattern. A pattern is a pair of regexs
                                  separated by ":", the former matching on
                                  metadata key, the latter on metadata value.
                                  The second regex is optional and defaults to
                                  ".*".
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
  -T, --type TYPE(S)              Only return entries of certain types.  Types
                                  are specified as a ","-separated list of
                                  type names; type names are: open, close,
                                  commodity, pad, balance, transaction, note,
                                  event, query, price, document, custom. The
                                  special value "all" means: all directive
                                  types. [default: transaction]
  --case-sensitive                Search case sensitively. Overrides:
                                  -i/--ignore-case and -S/--smart-case.
  -i, --ignore-case               Search case insensitively. Overrides
                                  -S/--smart-case; overridden by --case-
                                  sensitive.
  -S, --smart-case                Search case insensitively if all criteria
                                  are lowercase, sensitively otherwise.
                                  Overridden by: --case-sensitive and
                                  -i/--ignore-case.  [default: True]
  --posting-tags-meta TEXT        Metadata key used to attach tags to
                                  transaction postings.  [default: tags]
  -q, --quiet / --no-quiet        Quiet, do not write anything to standard
                                  output. Exit successfully immediately if any
                                  match is found.  [default: no-quiet]
  --skip-internals / --no-skip-internals
                                  When matching, ignore internal information
                                  not visible in the ledger. This includes the
                                  automatic metadata: ['filename', 'lineno']
                                  [default: skip-internals]
  -v, --invert-match              Invert the sense of matching: return entries
                                  that do *not* match given criteria. This
                                  clears the default "--type transaction"
                                  criteria, to avoid only returning non-
                                  transaction entries by default.
  --verbose                       Increase logging verbosity. Default
                                  verbosity is at WARNING level; passing this
                                  option once will increase it to INFO, twice
                                  or more to DEBUG.
  -V, --version                   Show the version and exit.
  --help                          Show this message and exit.

  Patterns:

  When given the "smart" PATTERN is interpreted according to the following
  heuristics, tried in order, first match wins:

  - if it is in the form "YYYY-MM-DD" -> then it is interpreted as --date

  - "#tag" -> --tag

  - "^link" -> --link

  - "@payee" -> --payee

  - if it starts with one of the five account types ("Assets", "Equity",
  "Expenses",   "Income", "Liabilities") -> --account

  - "key:value" -> --metadata

  - otherwise -> --somewhere

  Exit status:

  Exit status is 0 (success) if a match is found, 1 if no match is found, 2 if
  an error occurred.
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

[gpl2-here]: https://github.com/zacchiro/beangrep/blob/main/LICENSE-GPL-2.0-or-later
[gpl2-home]: https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
