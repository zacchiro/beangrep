BEANGREP_BIN = bin/bean-grep
BEANGREP_PY = src/beangrep/beangrep.py
MAN_EXTRAS = data/man-extras.h2m
MAN_PAGE = src/beangrep/data/bean-grep.1
README = README.md
UPDATE_README = dev/update-readme

all: $(README) $(MAN_PAGE)

build: all
	python3 -m build

$(README): $(BEANGREP_PY) $(UPDATE_README) Makefile
	$(UPDATE_README)

$(MAN_PAGE): $(BEANGREP_PY) $(MAN_EXTRAS) Makefile
	help2man $(BEANGREP_BIN) --name "grep-like filter for Beancount" --no-info --include $(MAN_EXTRAS) --output $@

clean:
	rm -f $(MAN_PAGE)

.PHONY: all build clean
