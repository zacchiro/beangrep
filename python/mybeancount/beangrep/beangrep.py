# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later

import beancount.loader
import click
import logging
import re

from beancount.core import data
from beancount.core.amount import Amount
from beancount.parser.printer import print_entry
from dataclasses import dataclass
from datetime import datetime
from decimal import Decimal
from enum import Enum
from typing import Optional, Self

# TODO add unit tests
# TODO make code mypy clean

DEFAULT_LEDGER = "current.beancount"

POSTING_TAGS_SEP = ","
KEY_VAL_SEP = ":"
POSTING_TAGS_META = "tags"
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
            case self.EQ.value:
                return lhs == rhs
            case self.LT.value:
                return lhs < rhs
            case self.GT.value:
                return lhs > rhs
            case self.LEQ.value:
                return lhs <= rhs
            case self.GEQ.value:
                return lhs >= rhs

    @classmethod
    def parse(cls, s: str) -> Self:
        match s:
            case "=":
                return cls.EQ
            case "<":
                return cls.LT
            case ">":
                return cls.GT
            case "<=":
                return cls.LEQ
            case ">=":
                return RelOp.GEQ
            case _:
                raise ValueError(f'invalid comparison operator "{s}"')


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

        return cls(**matches)


@dataclass
class DatePredicate:
    """Predicate on dates, filtering on year/month/day."""

    comp: RelOp = RelOp.EQ
    year: Optional[int] = None
    month: Optional[int] = None
    day: Optional[int] = None

    # TODO BUG: checking field by field does not work, because it requires that each
    # field is individually larger/smaller/equal than its peer. Cf. failing tests.
    def match(self, date: datetime) -> bool:
        """Test if a date matches the date predicate."""
        is_match = True
        if is_match and self.year is not None:
            is_match = is_match and self.comp.eval(date.year, self.year)
        if is_match and self.month is not None:
            is_match = is_match and self.comp.eval(date.month, self.month)
        if is_match and self.day is not None:
            is_match = is_match and self.comp.eval(date.day, self.day)

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

        return cls(**matches)


@dataclass
class Criteria:
    """Criteria to select matching Beancount entries."""

    account: Optional[re.Pattern] = None
    amount: Optional[list[AmountPredicate]] = None
    date: Optional[list[DatePredicate]] = None
    metadata: Optional[tuple[re.Pattern, re.Pattern]] = None
    narration: Optional[re.Pattern] = None
    payee: Optional[re.Pattern] = None
    tag: Optional[re.Pattern] = None
    types: Optional[set[type]] = None


def get_accounts(entry: data.Directive) -> set[str]:
    """Extract account names referenced from a Beancount directive.

    - For transactions, return the name of all accounts in postings.

    - For directives like open, close, etc. return the singleton name of the
      referenced account.

    """
    accounts = []
    match type(entry):
        case (
            data.Open | data.Close | data.Pad | data.Balance | data.Note | data.Document
        ) if entry.account is not None:
            accounts.append(entry.account)
        case data.Transaction:
            accounts.extend(p.account for p in entry.postings)

    return set(accounts)


def get_amounts(entry: data.Directive) -> set[Amount]:
    """Extract all amounts present in a Beancount directive.

    Amounts are extracted from directives of the following types:

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


def get_metadata(entry: data.Directive) -> set[tuple[str, str]]:
    """Extract all key/value metadata pairs attached to a Beancount directive."""

    metadata = list(getattr(entry, "meta", {}).items())

    if isinstance(entry, data.Transaction):
        for posting in entry.postings:
            metadata.extend(getattr(posting, "meta", {}).items())

    # Remove metadata with dunder keys (e.g., __tolerances__) as they contain unhashable
    # values, and impossible to properly filter anyway.
    metadata = set((k, v) for (k, v) in metadata if not k.startswith("__"))

    return metadata


def get_tags(entry: data.Directive, posting_tags_meta=POSTING_TAGS_META) -> set[str]:
    """Extract all tags applied to a Beancount directive.

    Returned tags include:

    - Tags applied globally to the directive.

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


def entry_matches(
    entry: data.Directive, criteria: Criteria, posting_tags_meta=POSTING_TAGS_META
) -> bool:
    """Check if a Beancount entry matches stated criteria."""

    predicates = []
    if criteria.account:
        predicates.append(
            lambda e: (any(re.search(criteria.account, a) for a in get_accounts(e)))
        )
    if criteria.amount:
        # Matching semantics: there exists at least one amount (outer any()) that
        # matches all (inner all()) amount predicates.
        lambda e: (
            # TODO BUG predicate always passes, which is obviously incorrect
            any(
                all(amount_pred.match(a) for amount_pred in criteria.amount)
                for a in get_amounts(e)
            )
        )
    if criteria.date:
        predicates.append(
            lambda e: (all(date_pred.match(e.date) for date_pred in criteria.date))
        )
    if criteria.metadata:
        (key_re, val_re) = criteria.metadata
        predicates.append(
            lambda e: any(
                re.search(key_re, k) and re.search(val_re, v)
                for (k, v) in get_metadata(e)
            )
        )
    if criteria.narration:
        predicates.append(
            lambda e: (
                hasattr(e, "narration") and re.search(criteria.narration, e.narration)
            )
        )
    if criteria.payee:
        predicates.append(
            lambda e: (hasattr(e, "payee") and re.search(criteria.payee, e.payee))
        )
    if criteria.tag:
        predicates.append(
            lambda e: (
                any(re.search(criteria.tag, t) for t in get_tags(e, posting_tags_meta))
            )
        )
    if criteria.types:
        predicates.append(lambda e: type(e) in criteria.types)

    is_match = all(p(entry) for p in predicates)
    return is_match


def parse_types(types_pat: str) -> set[type]:
    """Parse a directive type pattern.

    A type pattern is a list of type names separated by TYPE_SEP, or the special value
    "all" to mean all directive types.

    """
    types: Optional[list[type]] = None

    def parse_type(s: str) -> type:
        match s:
            case "open":
                return data.Open
            case "close":
                return data.Close
            case "commodity":
                return data.Commodity
            case "pad":
                return data.Pad
            case "balance":
                return data.Balance
            case "transaction":
                return data.Transaction
            case "note":
                return data.Note
            case "event":
                return data.Event
            case "query":
                return data.Query
            case "price":
                return data.Price
            case "document":
                return data.Document
            case "custom":
                return data.Custom
            case _:
                raise ValueError(f'unknown directive type "{s}"')

    if types_pat == "all":
        # List from beancount.core.data.Directive union. To be kept in sync.
        types = [
            data.Open,
            data.Close,
            data.Commodity,
            data.Pad,
            data.Balance,
            data.Transaction,
            data.Note,
            data.Event,
            data.Query,
            data.Price,
            data.Document,
            data.Custom,
        ]
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
        if KEY_VAL_SEP not in metadata_pat:
            raise ValueError(
                f'invalid metadata pattern "{metadata_pat}" '
                f"(does not contain key/value separator '{KEY_VAL_SEP}')"
            )
        (key_re, val_re) = metadata_pat.split(KEY_VAL_SEP, maxsplit=1)
        criteria.metadata = (re_compile(key_re), re_compile(val_re))
    if narration_re is not None:
        criteria.narration = re_compile(narration_re)
    if payee_re is not None:
        criteria.payee = re_compile(payee_re)
    if tag_re is not None:
        criteria.tag = re_compile(tag_re)
    if type_pat is not None:
        criteria.types = parse_types(type_pat)

    return criteria


def filter_entries(entries, criteria, posting_tags_meta=POSTING_TAGS_META):
    """Filter entries to only return those that match criteria."""
    for entry in entries:
        if entry_matches(entry, criteria, posting_tags_meta):
            logging.debug("Entry %s matches criteria, keeping it", entry)
            yield entry
        else:
            logging.debug("Entry %s does not matche criteria, skipping it", entry)


@click.command()
@click.argument(
    # TODO add support for reading from stdin, either implicitly or passing "-"
    "filename",
    type=click.Path(exists=True, dir_okay=False),
    default=DEFAULT_LEDGER,
)
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
    metavar="[OP]AMOUNT [REGEX]",
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
    # TODO make the second regex optional, easing filtering on metadata keys only
    metavar="REGEX:REGEX",
    help="Only return entries with at least one metadata key/value pair matching "
    "given pattern. A pattern is a pair of regular expression separated by ':', "
    "the former matching on metadata key, the latter on metadata value.",
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
    "-i",
    "--ignore-case/--no-ignore-case",
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
    tag_re,
    type_pat,
    ignore_case,
    posting_tags_meta,
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

    ledger = beancount.loader.load_file(filename)
    try:
        criteria = _build_criteria(
            account_re=account_re,
            amount_preds=amount_preds,
            date_preds=date_preds,
            narration_re=narration_re,
            metadata_pat=metadata_pat,
            payee_re=payee_re,
            tag_re=tag_re,
            type_pat=type_pat,
            ignore_case=ignore_case,
        )
    except ValueError as e:
        raise click.UsageError(e.args[0]) from e
    logging.info("Using search criteria: %s", criteria)
    for entry in filter_entries(ledger[0], criteria, posting_tags_meta):
        print_entry(entry)


if __name__ == "__main__":
    cli()