# stdlib tools

I constantly forget how to use the various tooling of [Stdlib](https://github.com/stdlib-js/stdlib), so here's lots of little notes in no particular order.

### Github bot

See [slash-commands.yml](https://github.com/stdlib-js/stdlib/blob/develop/.github/workflows/slash_commands.yml) for commands including

```bash
/stdlib [help check-files update-copyright-years 
  lint-autofix merge rebase]
```


### Things to check when authoring a module

Do tests run?

```bash
make TESTS_FILTER=".*/lapack/base/zlacgv/.*" test
```


Do examples run?

```bash
make EXAMPLES_FILTER=".*/lapack/base/zlacgv/.*" examples-c
make EXAMPLES_FILTER=".*/lapack/base/zlacgv/.*" examples
```

Do benchmarks run?

```bash
make BENCHMARKS_FILTER=".*/lapack/base/zlacgv/.*" benchmark
```

Does it lint?

```bash
make lint-editorconfig-files FILES="lib/node_modules/@stdlib/lapack/base/zlacgv/src/zlacgv.f"
```

Do native add-ons build?

```bash
make install-node-addons NODE_ADDONS_PATTERN="lapack/base/zlacgv"
```

### Compile native add-on with BLAS

```bash
BLAS=apple_accelerate make install-node-addons NODE_ADDONS_PATTERN="..."
```
