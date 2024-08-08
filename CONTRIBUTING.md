Before submitting patches, make sure to regenerate all generated files (e.g.,
`README.md`), that the entire test suite passes, and that the code base is
lint-clean. One easy way to do all of the above at once is to run:

```
$ make all check test
```

Which requires [GNU Make](https://www.gnu.org/software/make/), as well as the
various tools used for linting, testing, etc. See the top-level `Makefile` for
details.
