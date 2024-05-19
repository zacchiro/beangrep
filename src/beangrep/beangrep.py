# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later
__copyright__ = "Copyright (C) 2024  Stefano Zacchiroli <zack@upsilon.cc>"
__license__ = "GPL-2.0-or-later"

import calendar
import logging
import re
from collections.abc import Iterable
from dataclasses import dataclass, field
from datetime import date, timedelta
from decimal import Decimal
from enum import Enum
from typing import Optional, Self, cast

from beancount.core import data  # type: ignore
from beancount.core.amount import Amount  # type: ignore

INTERNALS_META = set(["filename", "lineno"])
KEY_VAL_SEP = ":"
META_VAL_RE = ".*"
POSTING_TAGS_META = "tags"
POSTING_TAGS_SEP = ","
SKIP_INTERNALS = True
TYPES_DEFAULT = frozenset([data.Transaction])
TYPE_SEP = ","


class Caseness(Enum):
    """Case-matching policy."""

    MATCH = 1
    IGNORE = 2
    SMART = 3

    @classmethod
    def from_cli(
        cls, case_sensitive: bool, ignore_case: bool, smart_case: bool
    ) -> Self:
        """Create a CaseMatching enum from CLI arguments, applying overrides."""
        if case_sensitive:
            result = cls.MATCH
        elif ignore_case:
            result = cls.IGNORE
        else:
            result = cls.SMART
        return cast(Self, result)

    def ignore_case(self, s: str) -> bool:
        """Check if caseness should be ignored for a given string."""
        result = False
        match self:
            case self.MATCH:
                result = False
            case self.IGNORE:
                result = True
            case self.SMART:
                if any(c.isupper() for c in s):
                    result = False
                else:
                    result = True
        return result


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

    accounts: list[re.Pattern] = field(default_factory=list)
    amounts: list[AmountPredicate] = field(default_factory=list)
    dates: list[DatePredicate] = field(default_factory=list)
    links: list[re.Pattern] = field(default_factory=list)
    metadatas: list[tuple[re.Pattern, re.Pattern]] = field(default_factory=list)
    narrations: list[re.Pattern] = field(default_factory=list)
    payees: list[re.Pattern] = field(default_factory=list)
    somewheres: list[re.Pattern] = field(default_factory=list)
    tags: list[re.Pattern] = field(default_factory=list)
    types: frozenset[type] = TYPES_DEFAULT

    @staticmethod
    def _re_compile(ignore_case: bool, s: str) -> re.Pattern:
        """Compile a regex, ignoring case or not."""
        re_flags = 0
        if ignore_case:
            re_flags = re.IGNORECASE

        return re.compile(s, flags=re_flags)

    @classmethod
    def guess(
        cls,
        pattern: str,
        caseness: Caseness = Caseness.SMART,
        base: Optional[Self] = None,
    ) -> Self:
        """Guess criteria from a single textual pattern, using heuristics.

        Heuristics (ordered by priority):

        - "YYYY-MM-DD" -> date criteria
        - "#tag" -> tag criteria
        - "^link" -> link criteria
        - "@payee" -> payee criteria
        - starting with an account type (Assets, Liabilities, etc.) -> account criteria
        - "key:value" -> metadata criteria
        - default -> somewhere criteria

        """
        criteria = base if base is not None else cls()
        ignore_case = caseness.ignore_case(pattern)

        if re.search(r"^\d{4}-\d{2}-\d{2}$", pattern):
            criteria.dates.append(DatePredicate.parse(f"={pattern}"))
        elif re.search(r"^#[-\w]+$", pattern):
            criteria.tags.append(cls._re_compile(ignore_case, pattern[1:]))
        elif re.search(r"^@.+$", pattern):
            criteria.payees.append(cls._re_compile(ignore_case, pattern[1:]))
        elif re.search(r"^\^[-\w]+$", pattern):
            criteria.links.append(cls._re_compile(ignore_case, pattern[1:]))
        elif re.search(r"^(Assets|Liabilities|Equity|Income|Expenses)", pattern):
            criteria.accounts.append(cls._re_compile(ignore_case, pattern))
        elif m := re.search(r"(?P<key>\w+):(?P<val>\w+)$", pattern):
            matches = m.groupdict()
            criteria.metadatas.append(
                (
                    cls._re_compile(ignore_case, matches["key"]),
                    cls._re_compile(ignore_case, matches["val"]),
                )
            )
        else:
            criteria.somewheres.append(cls._re_compile(ignore_case, pattern))

        return criteria

    @classmethod
    def from_cli(
        cls,
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
        caseness,
        invert_match,
        base: Optional[Self] = None,
    ) -> Self:
        """Build criteria from command line arguments."""
        criteria = base if base is not None else cls()
        if invert_match and criteria.types == TYPES_DEFAULT:
            # clear "--type transaction" default if "--invert-match"
            criteria.types = frozenset()

        if accounts is not None:
            criteria.accounts.extend(
                cls._re_compile(caseness.ignore_case(s), s) for s in accounts
            )
        if amounts is not None:
            criteria.amounts.extend(AmountPredicate.parse(s) for s in amounts)
        if dates is not None:
            criteria.dates.extend(DatePredicate.parse(s) for s in dates)
        if links is not None:
            criteria.links.extend(
                cls._re_compile(caseness.ignore_case(s), s) for s in links
            )
        if metadatas is not None:
            for s in metadatas:
                ignore_case = caseness.ignore_case(s)
                if KEY_VAL_SEP in s:
                    (key_re, val_re) = s.split(KEY_VAL_SEP, maxsplit=1)
                else:
                    (key_re, val_re) = (s, META_VAL_RE)
                criteria.metadatas.append(
                    (
                        cls._re_compile(ignore_case, key_re),
                        cls._re_compile(ignore_case, val_re),
                    )
                )
        if narrations is not None:
            criteria.narrations.extend(
                cls._re_compile(caseness.ignore_case(s), s) for s in narrations
            )
        if payees is not None:
            criteria.payees.extend(
                cls._re_compile(caseness.ignore_case(s), s) for s in payees
            )
        if somewheres is not None:
            criteria.somewheres.extend(
                cls._re_compile(caseness.ignore_case(s), s) for s in somewheres
            )
        if tags is not None:
            criteria.tags.extend(
                cls._re_compile(caseness.ignore_case(s), s) for s in tags
            )
        if types is not None:
            # Note: we *override* here, rather than merging, because types are not meant
            # to be incrementally specified.
            criteria.types = frozenset(parse_types(types))

        return criteria


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
        case data.Balance | data.Price:
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


def get_links(entry: data.Directive) -> set[str]:
    """Return the links of an entry, or the empty set if missing."""
    return set(getattr(entry, "links", set()))
    # note: without outer `set()` in some cases we would return frozenset-s


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
    strings = strings.union(get_links(entry))
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


def account_matches(entry: data.Directive, criteria: Iterable[re.Pattern]) -> bool:
    """Check if a Beancount entry matches account criteria."""
    return all(any(pat.search(a) for a in get_accounts(entry)) for pat in criteria)


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


def link_matches(entry: data.Directive, criteria: Iterable[re.Pattern]) -> bool:
    """Check if a Beancount entry matches links criteria."""
    return all(any(pat.search(link) for link in get_links(entry)) for pat in criteria)


def metadata_matches(
    entry: data.Directive,
    criteria: Iterable[tuple[re.Pattern, re.Pattern]],
    skip_internals: bool = SKIP_INTERNALS,
) -> bool:
    """Check if a Beancount entry matches metadata criteria."""
    return all(
        any(
            key_pat.search(k) and val_pat.search(str(v))
            for (k, v) in get_metadata(entry, skip_internals=skip_internals)
        )
        for (key_pat, val_pat) in criteria
    )


def narration_matches(entry: data.Directive, criteria: Iterable[re.Pattern]) -> bool:
    """Check if a Beancount entry matches narration criteria."""
    return (s := get_narration(entry)) is not None and all(
        bool(pat.search(s)) for pat in criteria
    )


def payee_matches(entry: data.Directive, criteria: Iterable[re.Pattern]) -> bool:
    """Check if a Beancount entry matches payee criteria."""
    return (s := get_payee(entry)) is not None and all(
        bool(pat.search(s)) for pat in criteria
    )


def somewhere_matches(
    entry: data.Directive,
    criteria: Iterable[re.Pattern],
    posting_tags_meta: str = POSTING_TAGS_META,
    skip_internals: bool = SKIP_INTERNALS,
) -> bool:
    """Check if a Beancount entry matches somewhere criteria."""
    return all(
        any(
            pat.search(s)
            for s in get_strings(
                entry,
                posting_tags_meta=posting_tags_meta,
                skip_internals=skip_internals,
            )
        )
        for pat in criteria
    )
    # return bool(criteria.search(printer.format_entry(entry)))


def tag_matches(
    entry: data.Directive,
    criteria: Iterable[re.Pattern],
    posting_tags_meta=POSTING_TAGS_META,
) -> bool:
    """Check if a Beancount entry matches tag criteria."""
    return all(
        any(pat.search(t) for t in get_tags(entry, posting_tags_meta))
        for pat in criteria
    )


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
    if criteria.accounts:
        predicates.append(lambda e: account_matches(e, criteria.accounts))
    if criteria.amounts:
        predicates.append(lambda e: amount_matches(e, criteria.amounts))
    if criteria.dates:
        predicates.append(lambda e: date_matches(e, criteria.dates))
    if criteria.links:
        predicates.append(lambda e: link_matches(e, criteria.links))
    if criteria.metadatas:
        predicates.append(
            lambda e: metadata_matches(
                e, criteria.metadatas, skip_internals=skip_internals
            )
        )
    if criteria.narrations:
        predicates.append(lambda e: narration_matches(e, criteria.narrations))
    if criteria.payees:
        predicates.append(lambda e: payee_matches(e, criteria.payees))
    if criteria.somewheres:
        predicates.append(
            lambda e: somewhere_matches(
                e, criteria.somewheres, posting_tags_meta, skip_internals
            )
        )
    if criteria.tags:
        predicates.append(lambda e: tag_matches(e, criteria.tags, posting_tags_meta))
    if criteria.types:
        predicates.append(lambda e: type_matches(entry, criteria.types))

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


def parse_types(types_pat: str) -> frozenset[type]:
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

    return frozenset(types)


def filter_entries(
    entries,
    criteria,
    invert_match: bool = False,
    posting_tags_meta=POSTING_TAGS_META,
    skip_internals=SKIP_INTERNALS,
):
    """Filter entries to only return those that match criteria."""
    for entry in entries:
        is_match = entry_matches(
            entry,
            criteria,
            posting_tags_meta=posting_tags_meta,
            skip_internals=skip_internals,
        )
        match (is_match, invert_match):
            case (True, False):
                logging.debug("Entry %s matches criteria, returning it", entry)
                yield entry
            case (False, False):
                logging.debug("Entry %s does not match criteria, skipping it", entry)
            case (True, True):
                logging.debug(
                    "Entry %s matches criteria, but invert match is on, skipping it",
                    entry,
                )
            case (False, True):
                logging.debug(
                    "Entry %s does not match criteria, but invert match is on, "
                    "returning it",
                    entry,
                )
                yield entry
