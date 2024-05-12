# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later
__copyright__ = "Copyright (C) 2024  Stefano Zacchiroli <zack@upsilon.cc>"
__license__ = "GPL-2.0-or-later"

import beancount.loader  # type: ignore
import calendar
import click
import logging
import re
import shutil
import sys

from beancount.core import data  # type: ignore
from beancount.core.amount import Amount  # type: ignore
from beancount.parser import printer  # type: ignore
from collections.abc import Iterable
from dataclasses import dataclass
from datetime import date, timedelta
from decimal import Decimal
from enum import Enum
from tempfile import NamedTemporaryFile
from typing import cast, Optional, Self

KEY_VAL_SEP = ":"
INTERNALS_META = set(["filename", "lineno"])
META_VAL_RE = ".*"
POSTING_TAGS_META = "tags"
POSTING_TAGS_SEP = ","
SKIP_INTERNALS = True
TYPE_SEP = "|"


class RelOp(Enum):
    """Relational comparison operator."""

    EQ = 1  # =
    LT = 2  # <
    GT = 3  # >
    LEQ = 4  # <=
    GEQ = 5  # <=

    def eval(self, lhs, rhs) -> bool:
        """Evaluate the relational operator on two values (left-hand side and right-hand
        side, respectively).

        """
        match self.value:
            case RelOp.EQ.value:
                result = lhs == rhs
            case RelOp.LT.value:
                result = lhs < rhs
            case RelOp.GT.value:
                result = lhs > rhs
            case RelOp.LEQ.value:
                result = lhs <= rhs
            case RelOp.GEQ.value:
                result = lhs >= rhs
        return result

    @classmethod
    def parse(cls, s: str) -> Self:
        match s:
            case "=":
                result = cls.EQ
            case "<":
                result = cls.LT
            case ">":
                result = cls.GT
            case "<=":
                result = cls.LEQ
            case ">=":
                result = cls.GEQ
            case _:
                raise ValueError(f'invalid comparison operator "{s}"')
        return cast(Self, result)


@dataclass
class AmountPredicate:
    """Predicate on amounts, filtering on number/currency."""

    comp: RelOp = RelOp.EQ
    number: Optional[Decimal] = None
    currency: Optional[re.Pattern] = None

    def match(self, amount: Amount) -> bool:
        """Test if an amount matches the amount predicate."""
        is_match = True
        if is_match and self.number is not None:
            is_match = is_match and self.comp.eval(amount.number, self.number)
        if is_match and self.currency is not None:
            is_match = is_match and self.currency == amount.currency

        return is_match

    # regular expression used to parse amount predicates
    SYNTAX_RE = re.compile(
        r"^(?P<comp><|<=|=|>|>=)?"
        r"(?P<number>-?\d+(\.\d+)?)"
        r"(\s+(?P<currency>[A-Z][A-Z\._\-]*))?$"
    )

    @classmethod
    def parse(cls, s: str) -> Self:
        """Parse an amount predicate from string."""

        m = cls.SYNTAX_RE.match(s)
        if not m:
            raise ValueError(f'invalid amount predicate "{s}"')

        # remove None matches to avoid overriding AmountPredicate defaults below
        matches = dict((k, v) for (k, v) in m.groupdict().items() if v is not None)
        if "comp" in matches:
            matches["comp"] = RelOp.parse(matches["comp"])
        if "number" in matches:
            matches["number"] = Decimal(matches["number"])

        return cls(**matches)  # type: ignore  # too dynamic to type properly


@dataclass
class DatePredicate:
    """Predicate on dates, filtering on year/month/day.

    Assumption: if day is not None, neither year nor month is None; if month is not
    None, year is not None. When built from the CLI parameter, this assumption is
    satisfied by construction by the input format "YYYY[-MM[-DD]]".

    """

    comp: RelOp = RelOp.EQ
    year: Optional[int] = None
    month: Optional[int] = None
    day: Optional[int] = None

    def date_range(self) -> tuple[Optional[date], Optional[date]]:
        """Return the min/max valid dates (inclusive) for the predicate.

        None is returned to denote that there is no min/max.

        """
        r: tuple[Optional[date], Optional[date]] = (None, None)
        match (self.year, self.month, self.day, self.comp):
            # constraints: year only
            case (y, None, None, RelOp.LT) if y is not None:
                r = (None, date(y - 1, 12, 31))
            case (y, None, None, RelOp.LEQ) if y is not None:
                r = (None, date(y, 12, 31))
            case (y, None, None, RelOp.EQ) if y is not None:
                r = (date(y, 1, 1), date(y, 12, 31))
            case (y, None, None, RelOp.GEQ) if y is not None:
                r = (date(y, 1, 1), None)
            case (y, None, None, RelOp.GT) if y is not None:
                r = (date(y + 1, 1, 1), None)

            # constraints: year and month
            case (y, m, None, RelOp.LT) if y is not None and m is not None:
                r = (None, date(y, m, 1) - timedelta(days=1))
            case (y, m, None, RelOp.LEQ) if y is not None and m is not None:
                r = (None, date(y, m, calendar.monthrange(y, m)[1]))
            case (y, m, None, RelOp.EQ) if y is not None and m is not None:
                r = (date(y, m, 1), date(y, m, calendar.monthrange(y, m)[1]))
            case (y, m, None, RelOp.GEQ) if y is not None and m is not None:
                r = (date(y, m, 1), None)
            case (y, m, None, RelOp.GT) if y is not None and m is not None:
                r = (
                    date(y, m, calendar.monthrange(y, m)[1]) + timedelta(days=1),
                    None,
                )

            # constraints: year, month, day
            # fmt: off
            case (y, m, d, RelOp.LT) \
                    if y is not None and m is not None and d is not None:
                r = (None, date(y, m, d) - timedelta(days=1))
            case (y, m, d, RelOp.LEQ) \
                    if y is not None and m is not None and d is not None:
                r = (None, date(y, m, d))
            case (y, m, d, RelOp.EQ) \
                    if y is not None and m is not None and d is not None:
                r = (date(y, m, d), date(y, m, d))
            case (y, m, d, RelOp.GEQ) \
                    if y is not None and m is not None and d is not None:
                r = (date(y, m, d), None)
            case (y, m, d, RelOp.GT) \
                    if y is not None and m is not None and d is not None:
                r = (date(y, m, d) + timedelta(days=1), None)
            # fmt: on

        return r

    def match(self, date: date) -> bool:
        """Test if a date matches the date predicate."""
        (min_date, max_date) = self.date_range()

        is_match = True
        if is_match and min_date is not None:
            is_match = is_match and min_date <= date
        if is_match and max_date is not None:
            is_match = is_match and date <= max_date

        return is_match

    # regular expression used to parse date predicates
    SYNTAX_RE = re.compile(
        r"^(?P<comp><|<=|=|>|>=)?(?P<year>\d{4})(-(?P<month>\d{2})(-(?P<day>\d{2}))?)?$"
    )

    @classmethod
    def parse(cls, s: str) -> Self:
        """Parse a date predicate from string."""
        m = cls.SYNTAX_RE.match(s)
        if not m:
            raise ValueError(f'invalid date predicate "{s}"')

        # remove None matches to avoid overriding DatePredicate defaults below
        matches = dict((k, v) for (k, v) in m.groupdict().items() if v is not None)
        if "comp" in matches:
            matches["comp"] = RelOp.parse(matches["comp"])
        for k in ("year", "month", "day"):
            if k in matches:
                matches[k] = int(matches[k])

        return cls(**matches)  # type: ignore  # too dynamic to type properly


@dataclass
class Criteria:
    """Criteria to select matching Beancount entries."""

    account: Optional[re.Pattern] = None
    amount: Optional[list[AmountPredicate]] = None
    date: Optional[list[DatePredicate]] = None
    metadata: Optional[tuple[re.Pattern, re.Pattern]] = None
    narration: Optional[re.Pattern] = None
    payee: Optional[re.Pattern] = None
    somewhere: Optional[re.Pattern] = None
    tag: Optional[re.Pattern] = None
    types: Optional[set[type]] = None

    # TODO add a criteria matching on ^links


def get_accounts(entry: data.Directive) -> set[str]:
    """Extract account names referenced from a Beancount entry.

    - For transactions, return the name of all accounts in postings.

    - For pad entries, return the names of both debit and credit accounts.

    - For entries like open, close, etc. return the singleton name of the referenced
      account.

    """
    accounts = []
    match type(entry):
        case (
            data.Open | data.Close | data.Balance | data.Note | data.Document
        ) if entry.account is not None:
            accounts.append(entry.account)
        case data.Pad:
            accounts.extend([entry.account, entry.source_account])
        case data.Transaction:
            accounts.extend(p.account for p in entry.postings)

    return set(accounts)


def get_amounts(entry: data.Directive) -> set[Amount]:
    """Extract all amounts present in a Beancount entry.

    Amounts are extracted from entries of the following types:

    - Transaction (one amount per Posting)
    - Balance
    - Price

    """
    amounts = []
    match type(entry):
        case (data.Balance | data.Price):
            if hasattr(entry, "amount"):
                amounts.append(entry.amount)
        case data.Transaction:
            amounts.extend(p.units for p in entry.postings)

    return set(amounts)


def get_dates(entry: data.Directive) -> set[date]:
    """Extract all dates present in a Beancount entry.

    Currently this is always a singleton with the entry date, but other dates can be
    returned in the future (e.g., those stored in custom metadata).

    """
    return set((entry.date,))


def get_narration(entry: data.Directive) -> Optional[str]:
    """Return the narration of an entry, or None if missing."""
    return getattr(entry, "narration", None)


def get_metadata(
    entry: data.Directive,
    skip_dunder: bool = True,
    skip_internals: bool = SKIP_INTERNALS,
    internals_meta: set[str] = INTERNALS_META,
) -> set[tuple[str, str]]:
    """Extract all key/value metadata pairs attached to a Beancount entry."""

    metadata = list(getattr(entry, "meta", {}).items())

    if isinstance(entry, data.Transaction):
        for posting in entry.postings:
            metadata.extend(getattr(posting, "meta", {}).items())

    if skip_dunder:
        # Remove metadata with dunder keys (e.g., __tolerances__) as they contain
        # unhashable values and are impossible to properly filter via the CLI anyway.
        metadata = [(k, v) for (k, v) in metadata if not k.startswith("__")]

    if skip_internals:
        metadata = [(k, v) for (k, v) in metadata if k not in internals_meta]

    return set(metadata)


def get_payee(entry: data.Directive) -> Optional[str]:
    """Return the payee of an entry, or None if missing."""
    return getattr(entry, "payee", None)


def get_tags(entry: data.Directive, posting_tags_meta=POSTING_TAGS_META) -> set[str]:
    """Extract all tags applied to a Beancount entry.

    Returned tags include:

    - Tags applied globally to the entry.

    - "Fake" tags applied to transaction postings (as Beancount syntax currently does
      not support tags on postings) using the value associated to the metadata key
      specified with posting_tags_meta. To disable looking for these tags pass
      posting_tags_meta=None.

    """
    tags = list(getattr(entry, "tags", []))

    if isinstance(entry, data.Transaction) and posting_tags_meta is not None:
        for posting in getattr(entry, "postings", []):
            tags.extend(
                tag.strip()
                for tag in posting.meta.get(posting_tags_meta, "").split(
                    POSTING_TAGS_SEP
                )
            )

    return set(tags)


def get_strings(
    entry: data.Directive,
    posting_tags_meta=POSTING_TAGS_META,
    skip_internals=SKIP_INTERNALS,
    internals_meta=INTERNALS_META,
) -> set[str]:
    """Extract all matchable strings from a Beancount entry.

    Strings extracted include: account names, amounts (converted to strings), dates
    (converted to strings), metadata (both keys and values), narrations and payees (for
    transactions), tags.

    """
    strings: set[str] = set()

    strings = strings.union(get_accounts(entry))
    strings = strings.union(str(a) for a in get_amounts(entry))
    strings = strings.union(str(d) for d in get_dates(entry))
    strings = strings.union(
        set(
            str(s)
            for pair in get_metadata(entry, skip_internals=skip_internals)
            for s in pair
        )
    )
    if (s := get_narration(entry)) is not None:
        strings.add(s)
    if (s := get_payee(entry)) is not None:
        strings.add(s)
    strings = strings.union(get_tags(entry, posting_tags_meta=posting_tags_meta))
    strings.add(TYPE_TO_STR[type(entry)])
    match type(entry):
        case data.Note:
            strings.add(entry.comment)
        case data.Event:
            strings = strings.union(set([entry.type, entry.description]))
        case data.Query:
            strings = strings.union(set([entry.name, entry.query_string]))
        case data.Document:
            strings.add(entry.filename)
        case data.Custom:
            strings.add(entry.type)
            strings = strings.union(set(str(v) for v in entry.values))

    return strings


def account_matches(entry: data.Directive, criteria: re.Pattern) -> bool:
    """Check if a Beancount entry matches account criteria."""
    return any(criteria.search(a) for a in get_accounts(entry))


def amount_matches(entry: data.Directive, criteria: Iterable[AmountPredicate]) -> bool:
    """Check if a Beancount entry matches amount criteria."""
    # Matching semantics: there exists at least one amount that matches all amount
    # predicates. Note: should return False for transactions with no amount.
    return bool(  # there is at least one amount...
        amounts := get_amounts(entry)
    ) and any(  # ... that matches all amount predicates
        all(amount_pred.match(a) for amount_pred in criteria) for a in amounts
    )


def date_matches(entry: data.Directive, criteria: Iterable[DatePredicate]) -> bool:
    """Check if a Beancount entry matches date criteria."""
    # Matching semantics: there exists at least one date that matches all date
    # predicates. Note: should return False for transactions with no dates.
    return bool(  # there is at least one date...
        dates := get_dates(entry)
    ) and any(  # ... that matches all amount predicates
        all(date_pred.match(a) for date_pred in criteria) for a in dates
    )


def metadata_matches(
    entry: data.Directive,
    criteria: tuple[re.Pattern, re.Pattern],
    skip_internals: bool = SKIP_INTERNALS,
) -> bool:
    """Check if a Beancount entry matches metadata criteria."""
    (key_re, val_re) = criteria
    return any(
        key_re.search(k) and val_re.search(str(v))
        for (k, v) in get_metadata(entry, skip_internals=skip_internals)
    )


def narration_matches(entry: data.Directive, criteria: re.Pattern) -> bool:
    """Check if a Beancount entry matches narration criteria."""
    return (s := get_narration(entry)) is not None and bool(criteria.search(s))


def payee_matches(entry: data.Directive, criteria: re.Pattern) -> bool:
    """Check if a Beancount entry matches payee criteria."""
    return (s := get_payee(entry)) is not None and bool(criteria.search(s))


def somewhere_matches(
    entry: data.Directive,
    criteria: re.Pattern,
    posting_tags_meta: str = POSTING_TAGS_META,
    skip_internals: bool = SKIP_INTERNALS,
) -> bool:
    """Check if a Beancount entry matches somewhere criteria."""
    return any(
        criteria.search(s)
        for s in get_strings(
            entry, posting_tags_meta=posting_tags_meta, skip_internals=skip_internals
        )
    )
    # return bool(criteria.search(printer.format_entry(entry)))


def tag_matches(
    entry: data.Directive, criteria: re.Pattern, posting_tags_meta=POSTING_TAGS_META
) -> bool:
    """Check if a Beancount entry matches tag criteria."""
    return any(criteria.search(t) for t in get_tags(entry, posting_tags_meta))


def type_matches(entry: data.Directive, types: Iterable[type]) -> bool:
    """Check if a Beancount entry matches type criteria."""
    return type(entry) in types


def entry_matches(
    entry: data.Directive,
    criteria: Criteria,
    posting_tags_meta=POSTING_TAGS_META,
    skip_internals=SKIP_INTERNALS,
) -> bool:
    """Check if a Beancount entry matches stated criteria."""

    predicates = []
    # ignoring types below because mypy fails to understand criteria fields are not None
    # inside lambdas, in spite of the explicit `is not None` guard
    if criteria.account is not None:
        predicates.append(lambda e: account_matches(e, criteria.account))  # type: ignore  # noqa:E501
    if criteria.amount is not None:
        predicates.append(lambda e: amount_matches(e, criteria.amount))  # type: ignore
    if criteria.date is not None:
        predicates.append(lambda e: date_matches(e, criteria.date))  # type: ignore
    if criteria.metadata is not None:
        predicates.append(
            lambda e: metadata_matches(
                e, criteria.metadata, skip_internals=skip_internals  # type: ignore
            )
        )
    if criteria.narration is not None:
        predicates.append(lambda e: narration_matches(e, criteria.narration))  # type: ignore  # noqa:E501
    if criteria.payee is not None:
        predicates.append(lambda e: payee_matches(e, criteria.payee))  # type: ignore
    if criteria.somewhere is not None:
        predicates.append(
            lambda e: somewhere_matches(
                e, criteria.somewhere, posting_tags_meta, skip_internals  # type: ignore
            )
        )
    if criteria.tag is not None:
        predicates.append(lambda e: tag_matches(e, criteria.tag, posting_tags_meta))  # type: ignore  # noqa:E501
    if criteria.types is not None:
        predicates.append(lambda e: type_matches(entry, criteria.types))  # type: ignore

    is_match = all(p(entry) for p in predicates)
    return is_match


STR_TO_TYPE: dict[str, type] = {
    "balance": data.Balance,
    "close": data.Close,
    "commodity": data.Commodity,
    "custom": data.Custom,
    "document": data.Document,
    "event": data.Event,
    "note": data.Note,
    "open": data.Open,
    "pad": data.Pad,
    "price": data.Price,
    "query": data.Query,
    "transaction": data.Transaction,
}
TYPE_TO_STR: dict[type, str] = dict((v, k) for (k, v) in STR_TO_TYPE.items())


def parse_types(types_pat: str) -> set[type]:
    """Parse a directive type pattern.

    A type pattern is a list of type names separated by TYPE_SEP, or the special value
    "all" to mean all directive types.

    """

    def parse_type(s: str) -> type:
        try:
            return STR_TO_TYPE[s]
        except KeyError:
            raise ValueError(f'unknown directive type "{s}"')

    types: list[type] = []
    if types_pat == "all":
        types = data.ALL_DIRECTIVES
    else:
        types = [parse_type(s) for s in types_pat.strip().split(TYPE_SEP)]

    return set(types)


def _build_criteria(
    account_re,
    amount_preds,
    date_preds,
    narration_re,
    metadata_pat,
    payee_re,
    somewhere_re,
    tag_re,
    type_pat,
    ignore_case,
) -> Criteria:
    """Build a Criteria object from command line arguments."""

    re_flags = 0
    if ignore_case:
        re_flags = re.IGNORECASE

    def re_compile(s):
        return re.compile(s, flags=re_flags)

    criteria = Criteria()

    if account_re is not None:
        criteria.account = re_compile(account_re)
    if amount_preds is not None:
        criteria.amount = list(map(AmountPredicate.parse, amount_preds))
    if date_preds is not None:
        criteria.date = list(map(DatePredicate.parse, date_preds))
    if metadata_pat is not None:
        if KEY_VAL_SEP in metadata_pat:
            (key_re, val_re) = metadata_pat.split(KEY_VAL_SEP, maxsplit=1)
        else:
            (key_re, val_re) = (metadata_pat, META_VAL_RE)
        criteria.metadata = (re_compile(key_re), re_compile(val_re))
    if narration_re is not None:
        criteria.narration = re_compile(narration_re)
    if payee_re is not None:
        criteria.payee = re_compile(payee_re)
    if somewhere_re is not None:
        criteria.somewhere = re_compile(somewhere_re)
    if tag_re is not None:
        criteria.tag = re_compile(tag_re)
    if type_pat is not None:
        criteria.types = parse_types(type_pat)

    return criteria


def filter_entries(
    entries,
    criteria,
    posting_tags_meta=POSTING_TAGS_META,
    skip_internals=SKIP_INTERNALS,
):
    """Filter entries to only return those that match criteria."""
    for entry in entries:
        if entry_matches(
            entry,
            criteria,
            posting_tags_meta=posting_tags_meta,
            skip_internals=skip_internals,
        ):
            logging.debug("Entry %s matches criteria, keeping it", entry)
            yield entry
        else:
            logging.debug("Entry %s does not match criteria, skipping it", entry)


@click.command(
    help="Search for entries matching given criteria in a Beancount ledger. "
    "Pretty print matching entries to standard output.\n\n"
    "The granularity of matching (and results) is that of individual entries, e.g., "
    "full transactions, balances, notes, etc. By default only transactions are returned"
    "; use the --type/-T option to override.\n\n"
    'To read from standard input, pass "-" as FILENAME, but beware that it implies '
    "on-disk buffering of stdin."
)
@click.argument("filename", required=True)
@click.option(
    "--account",
    "-a",
    "account_re",
    metavar="REGEX",
    help="Only return entries referencing accounts with names matching given regex.",
)
@click.option(
    "--amount",
    "-A",
    "amount_preds",
    metavar='"[OP]AMOUNT [REGEX]"',
    multiple=True,
    help="Only return entries with amounts matching the given amount predicate. "
    "An amount predicate start with an optional comparison operator "
    "(one of '<', '<=', '=', '>=', '>', with '=' being the default), "
    "followed by a decimal number (using '.' as decimal separator), "
    "followed by an optional currency regex. "
    "Multiple amount predicates can be given to express complex amount ranges.",
)
@click.option(
    "--date",
    "-d",
    "date_preds",
    metavar="[OP]DATE",
    multiple=True,
    help="Only return entries with dates matching the given date predicate. "
    "A date predicate start with an optional comparison operator "
    "(one of '<', '<=', '=', '>=', '>', with '=' being the default), "
    "and is followed by a date in the form YYYY-[MM[-DD]]. "
    "Multiple date predicates can be given to express complex date ranges.",
)
@click.option(
    "--meta",
    "--metadata",
    "-m",
    "metadata_pat",
    metavar="REGEX[:REGEX]",
    help="Only return entries with at least one metadata key/value pair matching "
    "given pattern. A pattern is a pair of regexs separated by ':', "
    "the former matching on metadata key, the latter on metadata value. "
    f"The second regex is optional and defaults to '{META_VAL_RE}'.",
)
@click.option(
    "--narration",
    "-n",
    "narration_re",
    metavar="REGEX",
    help="Only return entries whose narrations match given regex.",
)
@click.option(
    "--payee",
    "-p",
    "payee_re",
    metavar="REGEX",
    help="Only return entries whose payees match given regex.",
)
@click.option(
    "--somewhere",
    "--anywhere",
    "-s",
    "somewhere_re",
    metavar="REGEX",
    help="Only return entries with a value in them, anywhere, matching given regex.",
)
@click.option(
    "--tag",
    "-t",
    "tag_re",
    metavar="REGEX",
    help="Only return entries with at least one tag matching given regex. "
    "The tag can be located anywhere.",
)
@click.option(
    "--type",
    "-T",
    "type_pat",
    default="transaction",
    show_default=True,
    metavar="TYPE(S)",
    help="Only return entries of certain types. "
    f"Types are specified as a '{TYPE_SEP}'-separated list of type names; "
    "type names are: open, close, commodity, pad, balance, transaction, "
    "note, event, query, price, document, custom. "
    "The special value 'all' means: all directive types.",
)
@click.option(
    "--ignore-case/--no-ignore-case",
    "-i",
    "ignore_case",
    default=False,
    show_default=True,
    help="Ignore case distinctions in string matches.",
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
    help="Quiet, do not write anything to standard output. "
    "Exit succesfully immediately if any match is found.",
)
@click.option(
    "--skip-internals/--no-skip-internals",
    "skip_internals",
    default=True,
    show_default=True,
    help="When matching, ignore internal information not visible in the ledger. "
    f"This includes the automatic metadata: {INTERNALS_META}",
)
@click.option(
    "-v",
    "--verbose",
    count=True,
    help="Increase logging verbosity. "
    "Default level is WARNING. "
    "Passing this option once (e.g., -v) will increase it to INFO, "
    "twice or more (e.g., -vv) to DEBUG.",
)
def cli(
    filename,
    account_re,
    amount_preds,
    date_preds,
    narration_re,
    metadata_pat,
    payee_re,
    somewhere_re,
    tag_re,
    type_pat,
    ignore_case,
    posting_tags_meta,
    quiet,
    skip_internals,
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

    if filename == "-":
        # Beancount does not support streaming reading, so to mimic Unix filter
        # semantics we read stdin to the end and store it to a temporary file.
        with NamedTemporaryFile(prefix="beangrep.", suffix=".beancount") as tmpfile:
            logging.info("Buffering stdin to temporary file %s...", tmpfile.name)
            shutil.copyfileobj(sys.stdin.buffer, tmpfile.file)
            tmpfile.flush()
            logging.info("Loading ledger from %s...", tmpfile.name)
            ledger = beancount.loader.load_file(tmpfile.name)
    else:
        logging.info("Loading ledger from %s...", filename)
        ledger = beancount.loader.load_file(filename)

    if ledger[1]:  # fail upon Beancount loading error(s)
        raise click.FileError(
            filename,
            "\nBeancount load error(s):\n" + "\n".join(str(err) for err in ledger[1]),
        )

    try:
        criteria = _build_criteria(
            account_re=account_re,
            amount_preds=(amount_preds if amount_preds else None),
            date_preds=(date_preds if amount_preds else None),
            narration_re=narration_re,
            metadata_pat=metadata_pat,
            payee_re=payee_re,
            somewhere_re=somewhere_re,
            tag_re=tag_re,
            type_pat=type_pat,
            ignore_case=ignore_case,
        )
    except ValueError as e:
        raise click.UsageError(e.args[0]) from e
    logging.info("Using search criteria: %s", criteria)
    logging.debug("Input ledger contains %d entries", len(ledger[0]))

    match_found = False
    for entry in filter_entries(
        ledger[0],
        criteria,
        posting_tags_meta=posting_tags_meta,
        skip_internals=skip_internals,
    ):
        match_found = True
        if quiet:
            break
        else:
            printer.print_entry(entry)

    exit_status = 0 if match_found else 1
    sys.exit(exit_status)


if __name__ == "__main__":
    cli()
