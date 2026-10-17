# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later
__copyright__ = "Copyright (C) 2024  Stefano Zacchiroli <zack@upsilon.cc>"
__license__ = "GPL-2.0-or-later"

import logging
import shutil
import sys
import os
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

STDIN_NAME = "(standard input)"


def report(*parts: str) -> None:
    """Report a problem on stderr, grep-style: "bean-grep: FILE: MESSAGE"."""
    click.echo(": ".join(["bean-grep", *parts]), err=True)


def format_error(err, loaded_name: str, display_name: str) -> str:
    """Format a Beancount error as "FILE[:LINE]: MESSAGE".

    FILE is the one Beancount reports, which may be an included file; display_name
    replaces it when it is the file loaded as loaded_name, or a pseudo-filename.

    """
    source = err.source or {}
    err_file = source.get("filename") or ""
    if err_file.startswith("<") or (  # pseudo-filename, e.g. "<load>"
        os.path.abspath(err_file) == os.path.abspath(loaded_name)
    ):
        err_file = display_name
    if lineno := source.get("lineno"):
        return f"{err_file}:{lineno}: {err.message}"
    return f"{err_file}: {err.message}"


@click.command(
    context_settings={"auto_envvar_prefix": "BEANGREP"},
    help="""Search for entries matching the given criteria in Beancount journals.
Pretty-print matching entries to standard output.

Search criteria can be specified with the options below and/or by providing an explicit
"smart" PATTERN. If given, PATTERN is interpreted as described below under "Patterns".
If not given, search criteria are defined by explicit options.

Multiple options, options given multiple times, and PATTERN(s) are logically joined
(AND-ed) together.

The granularity of matching (and results) is that of individual entries, e.g., full
transactions, balances, notes, etc. By default, only transactions are returned; use the
--type/-T option to override.

To read from standard input, pass "-" as FILENAME, but beware that it implies on-disk
buffering of stdin.""",
    epilog="""Patterns:

When given, the "smart" PATTERN is interpreted according to the following heuristics,
tried in order (the first match wins):

- if it is in the form "YYYY-MM-DD" -> --date

- "#tag" -> --tag, if tag is made only of characters allowed in Beancount tags
(letters, digits, "-", "_", "/", "."); use --tag for arbitrary tag regexes

- "^link" -> --link, with the same restriction as tags; use --link for arbitrary link
regexes

- "@payee" -> --payee

- if it starts with one of the five account types ("Assets", "Equity", "Expenses",
"Income", "Liabilities"), capitalized as shown -> --account; e.g., "assets:cash" is
not an account search, but a metadata one

- "key:value" -> --metadata, if key is a valid Beancount metadata key (a lowercase
letter, followed by letters, digits, "-", or "_"); value is any regex without whitespace
and not starting with "//" (so that, e.g., URLs are --somewhere searches), possibly
empty ("key:") to match any value; use --metadata for values containing spaces

- otherwise -> --somewhere


Environment:

All options can be set via environment variables, which is most useful if you want to
override the defaults. Each environment variable consists of the prefix BEANGREP_
followed by the option name in uppercase, e.g., BEANGREP_VERBOSE,
BEANGREP_CASE_SENSITIVE, BEANGREP_QUIET, BEANGREP_NO_SKIP_INTERNALS, etc. Boolean
options are enabled/disabled by setting the corresponding variables to 1/0,
respectively, e.g., BEANGREP_QUIET=1.

The FILENAME can alternatively be specified via the environment variable
BEANCOUNT_FILENAME. This avoids having to specify it explicitly every time bean-grep is
run. Only a single FILENAME can be specified this way. When BEANCOUNT_FILENAME is set, a
single argument is always interpreted as PATTERN, and passing more than one argument is
an error; to search other files, unset the variable
(e.g., "BEANCOUNT_FILENAME= bean-grep PATTERN FILENAME...").


Exit status:

The exit status is 0 (success) if a match is found, 1 if no match is found, and 2 if an
error occurred. An error that occurs on some files does not stop processing the
remaining ones, but still results in exit status 2, unless --quiet is used and a match
is found.""",
)
@click.argument(
    "args",
    nargs=-1,
    metavar="[PATTERN] FILENAME...",  # override metavar to show what is required
)
@click.option(
    "--account",
    "-a",
    "accounts",
    metavar="REGEX",
    multiple=True,
    help="""Only return entries referencing accounts with names matching the given
regex.""",
)
@click.option(
    "--amount",
    "-A",
    "amounts",
    metavar='"[OP]AMOUNT [REGEX]"',
    multiple=True,
    help="""Only return entries with amounts matching the given amount predicate. An
amount predicate starts with an optional comparison operator (one of "<", "<=", "=",
">=", ">", with "=" being the default), followed by a decimal number (using "." as
decimal separator), followed by an optional currency regex. A predicate made of a
currency regex alone (e.g., "USD|EUR") matches amounts in any matching currency. Unlike
other regexes, which match anywhere in a value, currency regexes must match the whole
currency name: "US." matches USD, but "USD" does not match IRAUSD (use ".*USD"). Multiple
amount predicates can be given to express complex amount ranges. Only posting units and
balance and price amounts are considered, not posting costs or prices.""",
)
@click.option(
    "--date",
    "-d",
    "dates",
    metavar="[OP]DATE",
    multiple=True,
    help="""Only return entries with dates matching the given date predicate. A date
predicate starts with an optional comparison operator (one of "<", "<=", "=", ">=", ">",
with "=" being the default), followed by a date in the form YYYY[-MM[-DD]].
Multiple date predicates can be given to express complex date ranges.""",
)
@click.option(
    "--flag",
    "-f",
    "flags",
    metavar="REGEX",
    multiple=True,
    help="""Only return transactions with at least one flag matching the given regex
    (usually a single character, properly escaped). Both transaction and posting flags
    are used for matching.""",
)
@click.option(
    "--link",
    "-l",
    "links",
    metavar="REGEX",
    multiple=True,
    help="Only return entries with at least one link matching the given regex.",
)
@click.option(
    "--meta",
    "--metadata",
    "-m",
    "metadatas",
    metavar="REGEX[:REGEX]",
    multiple=True,
    help=f"""Only return entries with at least one metadata key/value pair matching
the given pattern. A pattern is a pair of regexes separated by ":", the former matching
the metadata key, the latter the metadata value. The second regex is optional and
defaults to "{META_VAL_RE}".""",
)
@click.option(
    "--narration",
    "-n",
    "narrations",
    metavar="REGEX",
    multiple=True,
    help="Only return entries whose narrations match the given regex.",
)
@click.option(
    "--payee",
    "-p",
    "payees",
    multiple=True,
    metavar="REGEX",
    help="Only return entries whose payees match the given regex.",
)
@click.option(
    "--somewhere",
    "--anywhere",
    "-s",
    "somewheres",
    metavar="REGEX",
    multiple=True,
    help="Only return entries with a value anywhere in them matching the given regex.",
)
@click.option(
    "--tag",
    "-t",
    "tags",
    metavar="REGEX",
    multiple=True,
    help="""Only return entries with at least one tag matching the given regex. The tag
can be located anywhere.""",
)
@click.option(
    "--type",
    "-T",
    "types",
    metavar="TYPE(S)",
    help=f"""Only return entries of certain types. Types are specified as a
"{TYPE_SEP}"-separated list of type names; valid type names are: open, close, commodity,
pad, balance, transaction, note, event, query, price, document, custom. The special
value "all" means all directive types. [default: transaction]""",
)
@click.option(
    "--case-sensitive",
    "case_sensitive",
    is_flag=True,
    default=False,
    help="Search case-sensitively. Overrides -i/--ignore-case and -S/--smart-case.",
)
@click.option(
    "--ignore-case",
    "-i",
    "ignore_case",
    is_flag=True,
    default=False,
    help="""Search case-insensitively. Overrides -S/--smart-case; overridden by
--case-sensitive.""",
)
@click.option(
    "--smart-case",
    "-S",
    "smart_case",
    is_flag=True,
    default=True,
    show_default=True,
    help="""Search case-insensitively if all criteria are lowercase, case-sensitively
otherwise. Overridden by --case-sensitive and -i/--ignore-case.""",
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
    help="""Quiet: do not write anything to standard output. Exit successfully
immediately if any match is found.""",
)
@click.option(
    "--skip-internals/--no-skip-internals",
    "skip_internals",
    default=True,
    show_default=True,
    help=f"""When matching, ignore internal information not visible in the ledger. This
includes the automatic metadata: {", ".join(sorted(INTERNALS_META))}.""",
)
@click.option(
    "--invert-match",
    "-v",
    "invert_match",
    is_flag=True,
    default=False,
    show_default=True,
    help="""Invert the sense of matching: return entries that do *not* match the
given criteria. This clears the default "--type transaction" criterion, to avoid only
returning non-transaction entries by default.""",
)
@click.option(
    "--verbose",
    count=True,
    help="""Increase logging verbosity. The default verbosity is WARNING; passing this
option once increases it to INFO, twice or more to DEBUG.""",
)
@click.version_option(None, "--version", "-V")
@click.pass_context
def cli(
    ctx,
    args,
    accounts,
    amounts,
    dates,
    flags,
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
    logger = logging.getLogger(__name__)
    logger.setLevel(log_level)
    logging.basicConfig(level=log_level)

    (pattern, filenames) = (None, [])
    # click has support for setting arguments via environment
    # variables but I can't figure out how to make it work with
    # beangrep's combination of optional pattern and variadic filenames.
    # So we just roll it ourselves.
    if os.getenv("BEANCOUNT_FILENAME"):
        if len(args) >= 2:
            raise click.BadArgumentUsage(
                "FILENAME arguments cannot be combined with BEANCOUNT_FILENAME;"
                " unset it to search other files."
            )
        filenames = [os.getenv("BEANCOUNT_FILENAME")]
        if len(args) == 1:
            pattern = args[0]
    else:
        if len(args) == 0:
            raise click.BadArgumentUsage("Must specify at least one Beancount file.")
        elif len(args) == 1:
            filenames = list(args)
        elif len(args) >= 2:
            (pattern, filenames) = (args[0], list(args[1:]))
    if len(list(filter(lambda fname: fname == "-", filenames))) > 1:
        raise click.BadArgumentUsage(
            'Standard input ("-") cannot be specified multiple times'
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
            flags=flags,
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
    logger.info("Using search criteria: %s", criteria)
    logger.info("Invert match is %s", "on" if invert_match else "off")

    match_found = False
    error_found = False
    for filename in filenames:
        display_name = STDIN_NAME if filename == "-" else filename
        loaded_name = filename  # name of the file actually loaded by Beancount
        try:
            if filename == "-":
                # Beancount does not support streaming input, so to mimic Unix filter
                # semantics we read stdin to the end and store it to a temporary file.
                with NamedTemporaryFile(
                    prefix="beangrep.", suffix=".beancount"
                ) as tmpfile:
                    logger.info(
                        'Buffering standard input to temporary file "%s"...',
                        tmpfile.name,
                    )
                    shutil.copyfileobj(sys.stdin.buffer, tmpfile.file)
                    tmpfile.flush()
                    loaded_name = tmpfile.name
                    logger.info('Loading entries from file "%s"...', tmpfile.name)
                    (entries, errors, _) = beancount.loader.load_file(tmpfile.name)
            else:
                logger.info('Loading entries from file "%s"...', filename)
                (entries, errors, _) = beancount.loader.load_file(filename)
        except OSError as e:  # e.g., directory or unreadable file
            report(display_name, e.strerror or str(e))
            error_found = True
            continue

        if errors and not entries:  # file could not be loaded at all, fail
            for err in errors:
                report(format_error(err, loaded_name, display_name))
            error_found = True
            continue
        elif errors:  # Beancount encountered loading error(s), warn user
            hint = "" if verbose else " (use --verbose to see them)"
            report(
                display_name,
                f"{len(errors)} Beancount error(s) when loading{hint}",
            )
            logger.info(
                "Beancount errors:\n"
                + "\n".join(format_error(e, loaded_name, display_name) for e in errors)
            )
        logger.debug('Loaded %d (valid) entries from file "%s"', len(entries), filename)

        for entry in filter_entries(
            entries,
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
        if quiet and match_found:  # exit immediately, skipping remaining files
            break

    if error_found and not (quiet and match_found):
        exit_status = 2
    else:
        exit_status = 0 if match_found else 1
    ctx.exit(exit_status)


if __name__ == "__main__":
    cli()
