# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later
__copyright__ = "Copyright (C) 2024  Stefano Zacchiroli <zack@upsilon.cc>"
__license__ = "GPL-2.0-or-later"

import logging
import shutil
import sys
from tempfile import NamedTemporaryFile

import beancount.loader  # type: ignore
import click
from beancount.parser import printer  # type: ignore

from .beangrep import (
    INTERNALS_META,
    META_VAL_RE,
    POSTING_TAGS_META,
    TYPE_SEP,
    Caseness,
    Criteria,
    filter_entries,
)


@click.command(
    help="""Search for entries matching given criteria in Beancount journals. Pretty
print matching entries to standard output.

Search criteria can be specified with the options below and/or providing an explicit
"smart" PATTERN. If given, PATTERN is interpreted as described below under "Patterns".
If not given, search criteria are defined by explicit options.

Multiple options, options given mutiple times, and PATTERN(s) are logically joined
(AND-ed) together.

The granularity of matching (and results) is that of individual entries, e.g., full
transactions, balances, notes, etc. By default only transactions are returned; use the
--type/-T option to override.

To read from standard input, pass "-" as FILENAME, but beware that it implies on-disk
buffering of stdin.""",
    epilog="""Patterns:

When given the "smart" PATTERN is interpreted according to the following heuristics,
tried in order, first match wins:

- if it is in the form "YYYY-MM-DD" -> then it is interpreted as --date

- "#tag" -> --tag

- "^link" -> --link

- "@payee" -> --payee

- if it starts with one of the five account types ("Assets", "Equity", "Expenses",
  "Income", "Liabilities") -> --account

- "key:value" -> --metadata

- otherwise -> --somewhere

Exit status:

Exit status is 0 (success) if a match is found, 1 if no match is found, 2 if an error
occurred.""",
)
@click.argument(
    "args",
    required=True,
    nargs=-1,
    metavar="[PATTERN] FILENAME...",  # override metavar to show what is required
)
@click.option(
    "--account",
    "-a",
    "accounts",
    metavar="REGEX",
    multiple=True,
    help="Only return entries referencing accounts with names matching given regex.",
)
@click.option(
    "--amount",
    "-A",
    "amounts",
    metavar='"[OP]AMOUNT [REGEX]"',
    multiple=True,
    help="""Only return entries with amounts matching the given amount predicate. An
amount predicate start with an optional comparison operator (one of "<", "<=", "=",
">=", ">", with "=" being the default), followed by a decimal number (using "." as
decimal separator), followed by an optional currency regex. Multiple amount predicates
can be given to express complex amount ranges.""",
)
@click.option(
    "--date",
    "-d",
    "dates",
    metavar="[OP]DATE",
    multiple=True,
    help="""Only return entries with dates matching the given date predicate. A date
predicate start with an optional comparison operator (one of "<", "<=", "=", ">=", ">",
with "=" being the default), and is followed by a date in the form YYYY-[MM[-DD]].
Multiple date predicates can be given to express complex date ranges.""",
)
@click.option(
    "--link",
    "-l",
    "links",
    metavar="REGEX",
    multiple=True,
    help="Only return entries with at least one link matching given regex.",
)
@click.option(
    "--meta",
    "--metadata",
    "-m",
    "metadatas",
    metavar="REGEX[:REGEX]",
    multiple=True,
    help=f"""Only return entries with at least one metadata key/value pair matching
given pattern. A pattern is a pair of regexs separated by ":", the former matching on
metadata key, the latter on metadata value. The second regex is optional and defaults to
"{META_VAL_RE}".""",
)
@click.option(
    "--narration",
    "-n",
    "narrations",
    metavar="REGEX",
    multiple=True,
    help="Only return entries whose narrations match given regex.",
)
@click.option(
    "--payee",
    "-p",
    "payees",
    multiple=True,
    metavar="REGEX",
    help="Only return entries whose payees match given regex.",
)
@click.option(
    "--somewhere",
    "--anywhere",
    "-s",
    "somewheres",
    metavar="REGEX",
    multiple=True,
    help="Only return entries with a value in them, anywhere, matching given regex.",
)
@click.option(
    "--tag",
    "-t",
    "tags",
    metavar="REGEX",
    multiple=True,
    help="""Only return entries with at least one tag matching given regex. The tag can
be located anywhere.""",
)
@click.option(
    "--type",
    "-T",
    "types",
    metavar="TYPE(S)",
    help=f"""Only return entries of certain types.  Types are specified as a
"{TYPE_SEP}"-separated list of type names; type names are: open, close, commodity, pad,
balance, transaction, note, event, query, price, document, custom. The special value
"all" means: all directive types. [default: transaction]""",
)
@click.option(
    "--case-sensitive",
    "case_sensitive",
    is_flag=True,
    default=False,
    help="Search case sensitively. Overrides: -i/--ignore-case and -S/--smart-case.",
)
@click.option(
    "--ignore-case",
    "-i",
    "ignore_case",
    is_flag=True,
    default=False,
    help="""Search case insensitively. Overrides -S/--smart-case; overridden by
--case-sensitive.""",
)
@click.option(
    "--smart-case",
    "-S",
    "smart_case",
    is_flag=True,
    default=True,
    show_default=True,
    help="""Search case insensitively if all criteria are lowercase, sensitively
otherwise. Overridden by: --case-sensitive and -i/--ignore-case.""",
)
@click.option(
    "--posting-tags-meta",
    default=POSTING_TAGS_META,
    show_default=True,
    help="Metadata key used to attach tags to transaction postings.",
)
@click.option(
    "--quiet/--no-quiet",
    "-q",
    "quiet",
    default=False,
    show_default=True,
    help="""Quiet, do not write anything to standard output. Exit successfully
immediately if any match is found.""",
)
@click.option(
    "--skip-internals/--no-skip-internals",
    "skip_internals",
    default=True,
    show_default=True,
    help=f"""When matching, ignore internal information not visible in the ledger. This
includes the automatic metadata: {sorted(INTERNALS_META)}""",
)
@click.option(
    "--invert-match",
    "-v",
    "invert_match",
    is_flag=True,
    default=False,
    show_default=True,
    help="""Invert the sense of matching: return entries that do *not* match given
criteria. This clears the default "--type transaction" criteria, to avoid only returning
non-transaction entries by default.""",
)
@click.option(
    "--verbose",
    count=True,
    help="""Increase logging verbosity. Default verbosity is at WARNING level; passing
this option once will increase it to INFO, twice or more to DEBUG.""",
)
@click.version_option(None, "--version", "-V")
@click.pass_context
def cli(
    ctx,
    args,
    accounts,
    amounts,
    dates,
    links,
    metadatas,
    narrations,
    payees,
    somewheres,
    tags,
    types,
    case_sensitive,
    ignore_case,
    smart_case,
    posting_tags_meta,
    quiet,
    skip_internals,
    invert_match,
    verbose,
):
    match verbose:
        case 0:
            log_level = logging.WARNING
        case 1:
            log_level = logging.INFO
        case _:  # >= 2
            log_level = logging.DEBUG
    logging.basicConfig(level=log_level)

    (pattern, filenames) = (None, [])
    if len(args) == 1:  # len(args) == 0 should not happen due to required=True
        filenames = list(args)
    elif len(args) >= 2:
        (pattern, filenames) = (args[0], list(args[1:]))
    if len(list(filter(lambda fname: fname == "-", filenames))) > 1:
        raise click.BadArgumentUsage(
            'Standard input ("-") cannot be specified multipled times'
        )

    caseness = Caseness.from_cli(case_sensitive, ignore_case, smart_case)
    try:
        criteria = Criteria()  # start from default criteria
        if pattern is not None:  # append smart pattern(s), if any
            criteria = Criteria.guess(pattern, caseness, base=criteria)
        criteria = Criteria.from_cli(  # append explicit options, if any
            accounts=accounts,
            amounts=amounts,
            dates=dates,
            links=links,
            metadatas=metadatas,
            narrations=narrations,
            payees=payees,
            somewheres=somewheres,
            tags=tags,
            types=types,
            caseness=caseness,
            invert_match=invert_match,
            base=criteria,
        )
    except ValueError as e:
        raise click.UsageError(e.args[0]) from e
    logging.info("Using search criteria: %s", criteria)
    logging.info("Invert match is %s", "on" if invert_match else "off")

    match_found = False
    for filename in filenames:
        if filename == "-":
            # Beancount does not support streaming reading, so to mimic Unix filter
            # semantics we read stdin to the end and store it to a temporary file.
            with NamedTemporaryFile(prefix="beangrep.", suffix=".beancount") as tmpfile:
                logging.info(
                    'Buffering standard input to temporary file "%s"...', tmpfile.name
                )
                shutil.copyfileobj(sys.stdin.buffer, tmpfile.file)
                tmpfile.flush()
                logging.info('Loading entries from file "%s"...', tmpfile.name)
                ledger = beancount.loader.load_file(tmpfile.name)
        else:
            logging.info('Loading entries from file "%s"...', filename)
            ledger = beancount.loader.load_file(filename)

        if ledger[1]:  # Beancount encountered loading error(s), warn user
            logging.warning(
                'Beancount encountered %d error(s) when loading file "%s"',
                len(ledger[1]),
                filename,
            )
            logging.info(
                "Beancount errors:\n" + "\n".join(str(err) for err in ledger[1])
            )
        if not ledger[0]:  # No (valid) entries in journal, fail
            ctx.fail(f'No valid entries found in file "{filename}"')
        else:
            logging.debug(
                'Loaded %d (valid) entries from file "%s"', filename, len(ledger[0])
            )

        for entry in filter_entries(
            ledger[0],
            criteria,
            invert_match=invert_match,
            posting_tags_meta=posting_tags_meta,
            skip_internals=skip_internals,
        ):
            match_found = True
            if quiet:
                break
            else:
                printer.print_entry(entry)

    exit_status = 0 if match_found else 1
    ctx.exit(exit_status)


if __name__ == "__main__":
    cli()
