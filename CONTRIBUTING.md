Before submitting patches, make sure that all generated files (e.g.,
`README.md`) are regenerated, that the entire test suite passes, and that the
code base is lint-clean. One easy way to do all of the above at once is to run:

```
$ make all check test
```

This requires [GNU Make](https://www.gnu.org/software/make/), as well as the
various tools used for linting, testing, etc. See the top-level `Makefile` for
details.
