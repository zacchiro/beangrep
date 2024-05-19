# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later
__copyright__ = "Copyright (C) 2024  Stefano Zacchiroli <zack@upsilon.cc>"
__license__ = "GPL-2.0-or-later"

from click.testing import CliRunner
from test_beangrep import SAMPLE_LEDGER, SAMPLE_LEDGER_SMALL

from beangrep import cli


def test_cli_basic():
    """Test basic CLI invocation."""
    runner = CliRunner()
    result = runner.invoke(cli, ["--help"])
    assert result.exit_code == 0
    assert result.output.startswith("Usage:")

    result = runner.invoke(cli, ["-V"])
    assert result.exit_code == 0
    assert " version " in result.output
    result = runner.invoke(cli, ["--version"])
    assert result.exit_code == 0
    assert " version " in result.output

    result = runner.invoke(cli, ["--foobarbazqux", SAMPLE_LEDGER])  # no such option
    assert result.exit_code == 2

    result = runner.invoke(cli, ["--date /2024", SAMPLE_LEDGER])  # invalid predicate
    assert result.exit_code == 2

    assert runner.invoke(cli, []).exit_code == 2
    assert runner.invoke(cli, ["pattern", "-", "-"]).exit_code == 2  # stdin twice


def test_cli_multiple_files():
    runner = CliRunner()
    result = runner.invoke(cli, ["Opening", SAMPLE_LEDGER, SAMPLE_LEDGER_SMALL])
    assert result.exit_code == 0
    assert "Assets:US:BofA:Checking" in result.output  # hit in SAMPLE_LEDGER
    assert "Assets:Checking" in result.output  # hit in SAMPLE_LEDGER_SMALL

    result = runner.invoke(cli, ["no such text", SAMPLE_LEDGER, SAMPLE_LEDGER_SMALL])
    assert result.exit_code == 1

    result = runner.invoke(cli, ["Opening Balance", SAMPLE_LEDGER, SAMPLE_LEDGER_SMALL])
    assert result.exit_code == 0  # hit in SAMPLE_LEDGER
    result = runner.invoke(cli, ["Opening balance", SAMPLE_LEDGER, SAMPLE_LEDGER_SMALL])
    assert result.exit_code == 0  # hit in SAMPLE_LEDGER_SMALL


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

    result = runner.invoke(cli, ["--link", "^a-day-in", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Mercadito" in result.output

    result = runner.invoke(
        cli, ["--metadata", "export:CASH", "--type", "commodity", SAMPLE_LEDGER]
    )
    assert result.exit_code == 0
    assert "US Dollar" in result.output

    result = runner.invoke(cli, ["--metadata", "export", SAMPLE_LEDGER])
    assert result.exit_code == 1
    result = runner.invoke(
        cli, ["--metadata", "export", "--type", "commodity", SAMPLE_LEDGER]
    )
    assert result.exit_code == 0
    assert "MUTF:VMMXX" in result.output

    result = runner.invoke(cli, ["--narration", "24kjhkg8sfjh2kjhkjh", SAMPLE_LEDGER])
    assert result.exit_code == 1
    result = runner.invoke(cli, ["--narration", "Buying groceries", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Onion Market" in result.output

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

    result = runner.invoke(cli, ["--type", "balance,query", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "taxes" in result.output
    assert "PreTax401k" in result.output

    result = runner.invoke(
        cli, ["--type", "pad,document,note,custom", SAMPLE_LEDGER_SMALL]
    )
    assert result.exit_code == 0
    assert "Rounding-Error" in result.output  # pad hit
    assert "statements.txt" in result.output  # document hit
    assert "YAY" in result.output  # note hit
    assert "TRUE" in result.output  # custom hit


def test_cli_invert_match():
    runner = CliRunner()
    result = runner.invoke(cli, ["-a", "Expenses:Grocery", SAMPLE_LEDGER_SMALL])
    assert result.exit_code == 0
    result = runner.invoke(cli, ["-a", "Expenses:Software", SAMPLE_LEDGER_SMALL])
    assert result.exit_code == 1
    result = runner.invoke(
        cli, ["--invert-match", "-n", "software", SAMPLE_LEDGER_SMALL]
    )
    assert result.exit_code == 0
    result = runner.invoke(cli, ["-v", "-s", ".", SAMPLE_LEDGER_SMALL])
    assert result.exit_code == 1


def test_cli_smart_pattern():
    runner = CliRunner()
    result = runner.invoke(cli, ["2014-03-28", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Buying groceries" in result.output

    result = runner.invoke(cli, ["#trip-chicago-2015", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Eataly Chicago" in result.output

    result = runner.invoke(cli, ["^a-day-in", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Mercadito" in result.output

    result = runner.invoke(cli, ["@RiverBank", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Paying the rent" in result.output

    result = runner.invoke(cli, ["Expenses:Vacation", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "vacation days" in result.output

    result = runner.invoke(cli, ["--type", "commodity", "export:CASH", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "US Dollar" in result.output

    result = runner.invoke(cli, ["Buying groceries", SAMPLE_LEDGER])
    assert result.exit_code == 0
    assert "Onion Market" in result.output
    result = runner.invoke(cli, ["24kjhkg8sfjh2kjhkjh", SAMPLE_LEDGER])
    assert result.exit_code == 1


def test_cli_case_matching():
    """Test --ignore-case flag."""
    runner = CliRunner()

    # Smart case:
    assert runner.invoke(cli, ["-p", "Uncle Boons", SAMPLE_LEDGER]).exit_code == 0
    assert runner.invoke(cli, ["-p", "uncle boons", SAMPLE_LEDGER]).exit_code == 0
    assert runner.invoke(cli, ["-S", "-p", "uncle boons", SAMPLE_LEDGER]).exit_code == 0
    assert runner.invoke(cli, ["-S", "-p", "Uncle Boons", SAMPLE_LEDGER]).exit_code == 0
    assert runner.invoke(cli, ["-S", "-p", "Uncle boons", SAMPLE_LEDGER]).exit_code == 1

    # Case sensitive:
    assert (
        runner.invoke(cli, ["--case-sensitive", "-p", "Boons", SAMPLE_LEDGER]).exit_code
        == 0
    )
    assert (
        runner.invoke(cli, ["--case-sensitive", "-p", "boons", SAMPLE_LEDGER]).exit_code
        == 1
    )

    # Case insensitive:
    assert runner.invoke(cli, ["-i", "-p", "uncle boons", SAMPLE_LEDGER]).exit_code == 0
    assert runner.invoke(cli, ["-i", "-p", "Uncle boons", SAMPLE_LEDGER]).exit_code == 0


def test_cli_conjunction():
    """Test logical conjunction of mutiple criteria."""
    runner = CliRunner()
    # Payee + date:
    assert (
        runner.invoke(cli, ["-p", "Hoogle", "-d", "2015-12", SAMPLE_LEDGER]).exit_code
        == 0
    )
    assert (
        runner.invoke(
            cli, ["-p", "Hoogle", "-d", "2015-12-04", SAMPLE_LEDGER]
        ).exit_code
        == 1
    )
    assert runner.invoke(cli, ["-d", "2015-12-04", SAMPLE_LEDGER]).exit_code == 0

    # Narration + narration:
    assert runner.invoke(cli, ["-n", "Sell", SAMPLE_LEDGER]).exit_code == 0
    assert runner.invoke(cli, ["-n", "Dividend", SAMPLE_LEDGER]).exit_code == 0
    assert (
        runner.invoke(cli, ["-n", "Sell shares", "-n", "ITOT", SAMPLE_LEDGER]).exit_code
        == 0
    )
    assert (
        runner.invoke(cli, ["-n", "Sell", "-n", "Dividend", SAMPLE_LEDGER]).exit_code
        == 1
    )


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


def test_cli_errors():
    runner = CliRunner()
    assert runner.invoke(cli, ["does_not_exist.beancount"]).exit_code == 2
    assert (
        runner.invoke(cli, ["--amount", "%23 USD", SAMPLE_LEDGER]).exit_code == 2
    )  # invalid amount predicate
