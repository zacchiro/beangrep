BEANGREP_BIN = bin/bean-grep
BEANGREP_PY = src/beangrep/beangrep.py
DIST_DIR = dist
MAN_EXTRAS = data/man-extras.h2m
MAN_PAGE = src/beangrep/data/bean-grep.1
README = README.md
PYTHON = python3
TWINE = $(PYTHON) -m twine
UPDATE_README = dev/update-readme

all: $(README) $(MAN_PAGE)

$(README): $(BEANGREP_PY) $(UPDATE_README) Makefile
	$(UPDATE_README)

$(MAN_PAGE): $(BEANGREP_PY) $(MAN_EXTRAS) Makefile
	help2man $(BEANGREP_BIN) --name "grep-like filter for Beancount" --no-info --include $(MAN_EXTRAS) --output $@

check:
	pre-commit run -a

coverage:
	dev/coverage

test:
	pytest

build release: all
	$(PYTHON) -m build

upload:
	$(MAKE) distclean
	$(MAKE) release
	$(TWINE) upload --repository pypi $(DIST_DIR)/*
test-upload: release
	$(MAKE) distclean
	$(MAKE) release
	$(TWINE) upload --repository testpypi $(DIST_DIR)/*

clean:
	rm -f $(MAN_PAGE)

distclean: clean
	rm -rf $(DIST_DIR)/

.PHONY: all build check clean coverage distclean release test test-upload upload
