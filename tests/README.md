# Zinc compiler tests

Run `python x.py build` then `python x.py test`. The harness typechecks with `--check-only` using `./out/stage1/bin/zinc`.

| Directory | Expectation |
|-----------|-------------|
| `compile-pass/` | `zinc --check-only` succeeds |
| `compile-fail/` | `zinc --check-only` fails |
