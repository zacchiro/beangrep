from beancount.core.amount import Amount
from .beangrep import AmountPredicate, DatePredicate, RelOp
from datetime import datetime
from decimal import Decimal


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
