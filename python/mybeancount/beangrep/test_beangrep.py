# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later

import beancount.loader
import re

from beancount.core import data
from beancount.core.amount import Amount
from .beangrep import AmountPredicate, Criteria, DatePredicate, RelOp
from .beangrep import filter_entries
from datetime import date, datetime
from decimal import Decimal

SAMPLE_LEDGER = "example.beancount"

_META = data.new_metadata("beangrep/test_beangrep.py", 1234)
_FLAG = "*"
_DATE = date(2024, 5, 10)


# def mk_empty_txn(date=_DATE, flag=_FLAG, meta=_META) -> data.Transaction:
#     """Test helper to create an empty transaction."""
#     return data.Transaction(
#         meta,
#         date,
#         flag,
#         None,
#         "something happened",
#         data.EMPTY_SET,
#         data.EMPTY_SET,
#         [],
#     )


# def mk_txn(
#     postings: Iterable[tuple[str, Union[str, Decimal], str]],
#     date=_DATE,
#     flag=_FLAG,
#     meta=_META,
# ) -> data.Transaction:
#     """Test helper to create a transaction with postings.
#
#     Postings are specified as (account, number, currency) triples.
#
#     """
#     txn = mk_empty_txn(date=date, flag=flag, meta=meta)
#     for account, number, currency in postings:
#         data.create_simple_posting(txn, account, number, currency)
#     return txn


def test_relop():
    assert RelOp.EQ.eval(42, 42)
    assert RelOp.LT.eval(41, 42)
    assert RelOp.GT.eval(43, 42)
    assert RelOp.LEQ.eval(41, 42)
    assert RelOp.LEQ.eval(42, 42)
    assert RelOp.GEQ.eval(42, 42)
    assert RelOp.GEQ.eval(43, 42)


def test_date_predicate():
    p = DatePredicate.parse
    assert p("=2024").match(datetime(2024, 5, 10))
    assert not p("2024").match(datetime(2025, 5, 10))
    # assert p("<2024-03").match(datetime(2024, 2, 10))  # TODO fails, should pass
    assert not p("<2024-03").match(datetime(2024, 3, 10))
    # assert p(">2023-01").match(datetime(2023, 2, 28))  # TODO fails, should pass
    assert p(">=2024-01-02").match(datetime(2024, 1, 3))
    assert p(">=2024-01-02").match(datetime(2024, 1, 2))
    assert not p(">=2024-01-02").match(datetime(2024, 1, 1))


def test_amount_predicate():
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


def load_sample_ledger(filename=SAMPLE_LEDGER):
    """Test helper to load a sample ledger from file."""
    return beancount.loader.load_file(filename)[0]


def grep_len(entries, criteria):
    """Shorthand to count the number of bean-grep results."""
    return len(list(filter_entries(entries, criteria)))


def test_account_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(account=re.compile("Y2013:US:State"))) == 28
    assert grep_len(l, Criteria(account=re.compile("US:Federal"))) == 94
    assert grep_len(l, Criteria(account=re.compile("^US:Federal$"))) == 0


def test_amount_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(amount=[AmountPredicate.parse("=3219.17 USD")])) == 2
    assert grep_len(l, Criteria(amount=[AmountPredicate.parse("3219.17")])) == 2
    assert grep_len(l, Criteria(amount=[AmountPredicate.parse("=3219.17 EUR")])) == 0
    assert grep_len(l, Criteria(amount=[AmountPredicate.parse("76.81 USD")])) == 1
    assert grep_len(l, Criteria(amount=[AmountPredicate.parse(">3000 USD")])) == 29
    assert grep_len(l, Criteria(amount=[AmountPredicate.parse("<1 USD")])) == 1177


def test_date_filtering():
    l = load_sample_ledger()  # noqa:E741  # TODO


def test_metadata_filtering():
    l = load_sample_ledger()  # noqa:E741  # TODO


def test_narration_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(narration=re.compile("Paying the rent"))) == 35
    assert grep_len(l, Criteria(narration=re.compile("paying the rent"))) == 0
    assert grep_len(l, Criteria(narration=re.compile("Transfering"))) == 9
    assert grep_len(l, Criteria(narration=re.compile("one year$"))) == 3
    assert grep_len(l, Criteria(narration=re.compile("^STATE"))) == 2


def test_payee_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(payee=re.compile("^Cafe"))) == 59
    assert grep_len(l, Criteria(payee=re.compile("Cafe Modagor"))) == 51
    assert grep_len(l, Criteria(payee=re.compile("Cafe Select$"))) == 8


def test_tag_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(tag=re.compile("^trip-new-york-2014$"))) == 45
    assert grep_len(l, Criteria(tag=re.compile("^trip"))) == 92
    assert grep_len(l, Criteria(tag=re.compile("asfgkjashfg"))) == 0


def test_type_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(types=data.ALL_DIRECTIVES)) == 2247
    assert grep_len(l, Criteria(types=[data.Transaction])) == 1146
    assert grep_len(l, Criteria(types=[data.Open])) == 60
    assert grep_len(l, Criteria(types=[data.Transaction, data.Open])) == 1146 + 60
    assert grep_len(l, Criteria(types=[data.Pad])) == 0
