# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later
__copyright__ = "Copyright (C) 2024  Stefano Zacchiroli <zack@upsilon.cc>"
__license__ = "GPL-2.0-or-later"

import re
from datetime import date
from decimal import Decimal
from pathlib import Path

import beancount.loader  # type: ignore
import pytest
from beancount.core import data  # type: ignore
from beancount.core.amount import Amount  # type: ignore

import beangrep
from beangrep import (
    TYPE_SEP,
    AmountPredicate,
    Criteria,
    DatePredicate,
    RelOp,
    filter_entries,
    parse_types,
)

DATA_DIR = Path(beangrep.__file__).parents[2] / "tests" / "data"
SAMPLE_LEDGER = str(DATA_DIR / "example.beancount")
SAMPLE_LEDGER_SMALL = str(DATA_DIR / "small.beancount")
DIRECTIVES_IN_SAMPLE = 2247  # `bean-quey example.beancount` shows this


def test_relop():
    """Test evaluation of relational operators."""
    assert RelOp.EQ.eval(42, 42)
    assert RelOp.LT.eval(41, 42)
    assert RelOp.GT.eval(43, 42)
    assert RelOp.LEQ.eval(41, 42)
    assert RelOp.LEQ.eval(42, 42)
    assert RelOp.GEQ.eval(42, 42)
    assert RelOp.GEQ.eval(43, 42)


def test_relop_parse_error():
    """Test relational operator parsing errors."""
    with pytest.raises(ValueError):
        assert RelOp.parse("!")
    with pytest.raises(ValueError):
        assert RelOp.parse("%")


def test_amount_predicate():
    """Test amount predicate matching."""
    p = AmountPredicate.parse
    assert p("42").match(Amount(Decimal(42), "EUR"))
    assert not p("=42").match(Amount(Decimal(41), "EUR"))
    assert p("42 EUR").match(Amount(Decimal(42), "EUR"))
    assert not p("42 EUR").match(Amount(Decimal(42), "USD"))
    assert p(">42").match(Amount(Decimal(43), "EUR"))
    assert not p(">42").match(Amount(Decimal(42), "EUR"))
    assert p("<=42 EUR").match(Amount(Decimal(42), "EUR"))
    assert p("<=42").match(Amount(Decimal(41), "EUR"))
    assert not p("<=42 EUR").match(Amount(Decimal(43), "EUR"))
    assert not p("<=42 EUR").match(Amount(Decimal(42), "USD"))


def test_amount_predicate_parse_error():
    """Test amount predicate parse errors."""
    with pytest.raises(ValueError):
        assert AmountPredicate.parse("==42")
    with pytest.raises(ValueError):
        assert AmountPredicate.parse("=42 EUR USD")


def test_date_predicate():
    """Test date predicate evaluation."""
    p = DatePredicate.parse
    assert p("=2024").match(date(2024, 5, 10))
    assert not p("2024").match(date(2025, 5, 10))
    assert p("<2024-03").match(date(2024, 2, 29))
    assert not p("<2024-03").match(date(2024, 3, 10))
    assert p(">2023-01").match(date(2023, 2, 28))
    assert p(">=2024-01-02").match(date(2024, 1, 3))
    assert p(">=2024-01-02").match(date(2024, 1, 2))
    assert not p(">=2024-01-02").match(date(2024, 1, 1))
    assert p("<=2024-01-01").match(date(2024, 1, 1))
    assert p("<=2024-01-01").match(date(2023, 12, 31))
    assert not p("<=2024-01-01").match(date(2024, 1, 2))
    assert not p("<2024-01-01").match(date(2024, 1, 1))
    assert p("<2024-01-01").match(date(2023, 12, 31))


def test_date_predicate_parse_error():
    """Test date predicate prasing errors."""
    with pytest.raises(ValueError):
        assert DatePredicate.parse("==2014")
    with pytest.raises(ValueError):
        assert DatePredicate.parse("=2014-01-01-01")
    with pytest.raises(ValueError):
        assert DatePredicate.parse("/2014-01")


def test_pattern_guessing():
    assert Criteria.guess("2024-05-15").dates == [DatePredicate.parse("=2024-05-15")]
    assert Criteria.guess("2024-05").dates == []
    assert Criteria.guess("2024").dates == []

    assert Criteria.guess("#some-tag").tags == [re.compile("some-tag", re.IGNORECASE)]
    assert Criteria.guess("some-tag").tags == []

    assert Criteria.guess("^some-link").links == [
        re.compile("some-link", re.IGNORECASE)
    ]
    assert Criteria.guess("some-link").links == []

    assert Criteria.guess("key:val").metadatas == [
        (
            re.compile("key", re.IGNORECASE),
            re.compile("val", re.IGNORECASE),
        )
    ]
    assert Criteria.guess("key:Val").metadatas == [
        (re.compile("key"), re.compile("Val"))
    ]
    assert Criteria.guess("key val").metadatas == []

    assert Criteria.guess("some text").somewheres == [
        (re.compile("some text", re.IGNORECASE))
    ]


def load_sample_ledger(filename=SAMPLE_LEDGER):
    """Test helper to load a sample ledger from file."""
    return beancount.loader.load_file(filename)[0]


def grep_len(entries, criteria):
    """Shorthand to count the number of bean-grep results."""
    return len(list(filter_entries(entries, criteria)))


def mk_criteria(**kwargs):
    """Create a Criteria object overriding the entry types default (unless explicitly
    present in kwargs), so that transactions are not selected by default.

    """
    if "types" not in kwargs:
        kwargs["types"] = None
    return Criteria(**kwargs)


def test_account_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, mk_criteria(accounts=[re.compile("Y2013:US:State")])) == 28
    assert grep_len(l, mk_criteria(accounts=[re.compile("US:Federal")])) == 94
    assert grep_len(l, mk_criteria(accounts=[re.compile("^US:Federal$")])) == 0


def test_amount_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert (
        grep_len(l, mk_criteria(amounts=[AmountPredicate.parse("=3219.17 USD")])) == 2
    )
    assert grep_len(l, mk_criteria(amounts=[AmountPredicate.parse("3219.17")])) == 2
    assert (
        grep_len(l, mk_criteria(amounts=[AmountPredicate.parse("=3219.17 EUR")])) == 0
    )
    assert grep_len(l, mk_criteria(amounts=[AmountPredicate.parse("76.81 USD")])) == 1
    assert grep_len(l, mk_criteria(amounts=[AmountPredicate.parse(">3000 USD")])) == 29
    assert grep_len(l, mk_criteria(amounts=[AmountPredicate.parse("<1 USD")])) == 1177
    assert (
        grep_len(
            l,
            mk_criteria(
                amounts=[
                    AmountPredicate.parse(">2800 USD"),
                    AmountPredicate.parse("<3000 USD"),
                ]
            ),
        )
        == 8
    )


def test_date_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("<1700")])) == 0
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("2013")])) == 739
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("2013-03")])) == 67
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("<=2013-12")])) == 765
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("2014")])) == 750
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("=2015")])) == 731
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("=2015-05-01")])) == 6
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse("=2030")])) == 1
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse(">=2029-08")])) == 1
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse(">2015-12-17")])) == 9
    assert grep_len(l, mk_criteria(dates=[DatePredicate.parse(">2031")])) == 0

    assert (
        grep_len(
            l,
            mk_criteria(
                dates=[DatePredicate.parse(">=2014"), DatePredicate.parse("<=2015")]
            ),
        )
        == 750 + 731
    )
    assert (
        grep_len(
            l,
            mk_criteria(
                dates=[DatePredicate.parse(">2013"), DatePredicate.parse("<2013")]
            ),
        )
        == 0
    )


def test_link_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, mk_criteria(links=[re.compile("day-in-sfo")])) == 2
    assert grep_len(l, mk_criteria(links=[re.compile("day-in-chicago")])) == 4
    assert grep_len(l, mk_criteria(links=[re.compile("^a-day-in-")])) == 2 + 4


def test_metadata_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert (
        grep_len(
            l, mk_criteria(metadatas=[(re.compile("^name$"), re.compile("US Dollar"))])
        )
        == 1
    )
    assert (
        grep_len(l, mk_criteria(metadatas=[(re.compile("^name$"), re.compile(".*"))]))
        == 9
    )
    assert (
        grep_len(
            l, mk_criteria(metadatas=[(re.compile("^name$"), re.compile("Vanguard"))])
        )
        == 3
    )
    assert (  # filename metadata are skipped by default, hence this returns 0
        grep_len(l, mk_criteria(metadatas=[(re.compile("filename"), re.compile(".*"))]))
        == 0
    )
    assert (  # passing skip_internals=False explicitly this time, to match on filename
        len(
            list(
                filter_entries(
                    l,
                    mk_criteria(metadatas=[(re.compile("filename"), re.compile(".*"))]),
                    skip_internals=False,
                )
            )
        )
        == DIRECTIVES_IN_SAMPLE
    )
    assert (
        grep_len(l, mk_criteria(metadatas=[(re.compile(".*"), re.compile("NYSEARC"))]))
        == 4
    )


def test_narration_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, mk_criteria(narrations=[re.compile("Paying the rent")])) == 35
    assert grep_len(l, mk_criteria(narrations=[re.compile("paying the rent")])) == 0
    assert grep_len(l, mk_criteria(narrations=[re.compile("Transfering")])) == 9
    assert grep_len(l, mk_criteria(narrations=[re.compile("one year$")])) == 3
    assert grep_len(l, mk_criteria(narrations=[re.compile("^STATE")])) == 2


def test_payee_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, mk_criteria(payees=[re.compile("^Cafe")])) == 59
    assert grep_len(l, mk_criteria(payees=[re.compile("Cafe Modagor")])) == 51
    assert grep_len(l, mk_criteria(payees=[re.compile("Cafe Select$")])) == 8


def test_somewhere_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert (
        grep_len(l, mk_criteria(somewheres=[re.compile("^Transfering")])) == 9
    )  # txn narration
    assert grep_len(l, mk_criteria(somewheres=[re.compile("^Cafe")])) == 59  # txn payee
    assert (
        grep_len(l, mk_criteria(somewheres=[re.compile("US:Federal")])) == 94
    )  # txn account
    assert (
        grep_len(l, mk_criteria(somewheres=[re.compile("trip-san")])) == 21
    )  # txn tag
    assert (
        grep_len(l, mk_criteria(somewheres=[re.compile("^a-day-in")])) == 6
    )  # txn link
    assert (
        grep_len(l, mk_criteria(somewheres=[re.compile("2015-05-01")])) == 6
    )  # txn date
    assert (
        grep_len(l, mk_criteria(somewheres=[re.compile("3219.17 USD")])) == 2
    )  # txn amount
    assert (
        grep_len(l, mk_criteria(somewheres=[re.compile("San Francisco")])) == 1
    )  # event description


def test_tag_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, mk_criteria(tags=[re.compile("^trip-new-york-2014$")])) == 45
    assert grep_len(l, mk_criteria(tags=[re.compile("^trip")])) == 92
    assert grep_len(l, mk_criteria(tags=[re.compile("asfgkjashfg")])) == 0


def test_type_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, mk_criteria(types=data.ALL_DIRECTIVES)) == DIRECTIVES_IN_SAMPLE
    assert grep_len(l, mk_criteria(types=[data.Transaction])) == 1146
    assert grep_len(l, mk_criteria(types=[data.Open])) == 60
    assert grep_len(l, mk_criteria(types=[data.Transaction, data.Open])) == 1146 + 60
    assert grep_len(l, mk_criteria(types=[data.Pad])) == 0


def test_parse_types():
    """Test parsing of entry type names."""
    assert set(parse_types("all")) == set(data.ALL_DIRECTIVES)
    assert set(parse_types(TYPE_SEP.join(["open", "close"]))) == set(
        [data.Open, data.Close]
    )
    assert set(
        parse_types(
            TYPE_SEP.join(["commodity", "balance", "transaction", "query", "price"])
        )
    ) == set([data.Commodity, data.Balance, data.Transaction, data.Query, data.Price])
    assert set(parse_types(TYPE_SEP.join(["pad", "note", "document"]))) == set(
        [data.Pad, data.Note, data.Document]
    )
    assert set(parse_types(TYPE_SEP.join(["event", "custom"]))) == set(
        [data.Event, data.Custom]
    )
    with pytest.raises(ValueError):
        parse_types("foo")
    with pytest.raises(ValueError):
        parse_types("^%@$#&")
