# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later
__copyright__ = "Copyright (C) 2024  Stefano Zacchiroli <zack@upsilon.cc>"
__license__ = "GPL-2.0-or-later"

import beancount.loader  # type: ignore
import beangrep
import re
import pytest

from beancount.core import data  # type: ignore
from beancount.core.amount import Amount  # type: ignore
from beangrep import (
    AmountPredicate,
    Criteria,
    DatePredicate,
    RelOp,
    TYPE_SEP,
    cli,
    filter_entries,
    parse_types,
)
from click.testing import CliRunner
from datetime import date
from decimal import Decimal
from pathlib import Path

SAMPLE_LEDGER = str(
    Path(beangrep.__file__).parents[2] / "tests" / "data" / "example.beancount"
)
DIRECTIVES_IN_SAMPLE = 2247  # `bean-quey example.beancount` shows this

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
    # TODO add tests for multiple amount predicates


def test_date_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(date=[DatePredicate.parse("<1700")])) == 0
    assert grep_len(l, Criteria(date=[DatePredicate.parse("2013")])) == 739
    assert grep_len(l, Criteria(date=[DatePredicate.parse("2013-03")])) == 67
    assert grep_len(l, Criteria(date=[DatePredicate.parse("<=2013-12")])) == 765
    assert grep_len(l, Criteria(date=[DatePredicate.parse("2014")])) == 750
    assert grep_len(l, Criteria(date=[DatePredicate.parse("=2015")])) == 731
    assert grep_len(l, Criteria(date=[DatePredicate.parse("=2015-05-01")])) == 6
    assert grep_len(l, Criteria(date=[DatePredicate.parse("=2030")])) == 1
    assert grep_len(l, Criteria(date=[DatePredicate.parse(">=2029-08")])) == 1
    assert grep_len(l, Criteria(date=[DatePredicate.parse(">2015-12-17")])) == 9
    assert grep_len(l, Criteria(date=[DatePredicate.parse(">2031")])) == 0

    assert (
        grep_len(
            l,
            Criteria(
                date=[DatePredicate.parse(">=2014"), DatePredicate.parse("<=2015")]
            ),
        )
        == 750 + 731
    )
    assert (
        grep_len(
            l,
            Criteria(date=[DatePredicate.parse(">2013"), DatePredicate.parse("<2013")]),
        )
        == 0
    )


def test_metadata_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert (
        grep_len(l, Criteria(metadata=(re.compile("^name$"), re.compile("US Dollar"))))
        == 1
    )
    assert grep_len(l, Criteria(metadata=(re.compile("^name$"), re.compile(".*")))) == 9
    assert (
        grep_len(l, Criteria(metadata=(re.compile("^name$"), re.compile("Vanguard"))))
        == 3
    )
    assert (  # filename metadata are skipped by default, hence this returns 0
        grep_len(l, Criteria(metadata=(re.compile("filename"), re.compile(".*")))) == 0
    )
    assert (  # passing skip_internals=False explicitly this time, to match on filename
        len(
            list(
                filter_entries(
                    l,
                    Criteria(metadata=(re.compile("filename"), re.compile(".*"))),
                    skip_internals=False,
                )
            )
        )
        == DIRECTIVES_IN_SAMPLE
    )
    assert (
        grep_len(l, Criteria(metadata=(re.compile(".*"), re.compile("NYSEARC")))) == 4
    )


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


def test_somewhere_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(somewhere=re.compile("trip-san"))) == 21
    assert grep_len(l, Criteria(somewhere=re.compile("San Francisco"))) == 1


def test_tag_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(tag=re.compile("^trip-new-york-2014$"))) == 45
    assert grep_len(l, Criteria(tag=re.compile("^trip"))) == 92
    assert grep_len(l, Criteria(tag=re.compile("asfgkjashfg"))) == 0


def test_type_filtering():
    l = load_sample_ledger()  # noqa:E741
    assert grep_len(l, Criteria(types=data.ALL_DIRECTIVES)) == DIRECTIVES_IN_SAMPLE
    assert grep_len(l, Criteria(types=[data.Transaction])) == 1146
    assert grep_len(l, Criteria(types=[data.Open])) == 60
    assert grep_len(l, Criteria(types=[data.Transaction, data.Open])) == 1146 + 60
    assert grep_len(l, Criteria(types=[data.Pad])) == 0


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


def test_cli_basic():
    """Test basic CLI invocation."""
    runner = CliRunner()

    result = runner.invoke(cli, ["--help"])
    assert result.exit_code == 0
    assert result.output.startswith("Usage:")

    result = runner.invoke(cli, ["--foobarbazqux", SAMPLE_LEDGER])  # no such option
    assert result.exit_code == 2

    result = runner.invoke(cli, ["--date /2024", SAMPLE_LEDGER])  # invalid predicate
    assert result.exit_code == 2


def test_cli_exit_code():
    """Test CLI exit code."""
    runner = CliRunner()
    result = runner.invoke(
        cli, ["--account", "Expenses:Food:Restaurant", SAMPLE_LEDGER]
    )
    assert result.exit_code == 0
    assert "Bar Crudo" in result.output

    result = runner.invoke(cli, ["--amount", "=76.81 USD", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Verizon Wireless" in result.output

    result = runner.invoke(cli, ["--date", "=2014-03-28", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Buying groceries" in result.output

    result = runner.invoke(
        cli, ["--metadata", "export:CASH", "--type", "commodity", SAMPLE_LEDGER]
    )
    assert result.exit_code == 0
    assert "US Dollar" in result.output

    result = runner.invoke(
        cli, ["--metadata", "export", "--type", "commodity", SAMPLE_LEDGER]
    )
    assert result.exit_code == 0
    assert "MUTF:VMMXX" in result.output

    result = runner.invoke(cli, ["--narration", "24kjhkg8sfjh2kjhkjh", SAMPLE_LEDGER])
    assert result.exit_code == 1

    result = runner.invoke(cli, ["--payee", "Uncle Boons", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Eating out" in result.output

    result = runner.invoke(cli, ["--somewhere", "trip-new-york", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Laut" in result.output

    result = runner.invoke(cli, ["--tag", "trip-chicago-2015", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Eataly Chicago" in result.output

    result = runner.invoke(cli, ["--type", "open", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Federal:PreTax401k" in result.output

    result = runner.invoke(cli, ["--type", "custom", SAMPLE_LEDGER])
    assert result.exit_code == 1

    result = runner.invoke(cli, ["--type", "balance|query", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "taxes" in result.output
    assert "PreTax401k" in result.output


def test_cli_ignore_case():
    """Test --ignore-case flag."""
    runner = CliRunner()
    assert runner.invoke(cli, ["-p", "Uncle Boons", SAMPLE_LEDGER]).exit_code == 0
    assert runner.invoke(cli, ["-p", "uncle boons", SAMPLE_LEDGER]).exit_code == 1
    assert runner.invoke(cli, ["-i", "-p", "uncle boons", SAMPLE_LEDGER]).exit_code == 0


def test_cli_quiet():
    """Test --quiet flag."""
    runner = CliRunner()

    result = runner.invoke(cli, ["--amount", "=76.81 USD", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "76.81 USD" in result.output

    result = runner.invoke(cli, ["--quiet", "--amount", "=76.81 USD", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "76.81 USD" not in result.output


def test_cli_stdin():
    """Test reading from stdin passing "-" as filename."""
    runner = CliRunner()
    assert runner.invoke(cli, ["-p", "Uncle Boons", SAMPLE_LEDGER]).exit_code == 0
    with open(SAMPLE_LEDGER) as f:
        ledger_text = f.read()
        assert (
            runner.invoke(cli, ["-p", "Uncle Boons", "-"], input=ledger_text).exit_code
            == 0
        )
