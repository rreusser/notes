# stdlib tools

I constantly forget how to use the various tooling of [Stdlib](https://github.com/stdlib-js/stdlib), so here's lots of little notes in no particular order.

### Github bot

See [slash-commands.yml](https://github.com/stdlib-js/stdlib/blob/develop/.github/workflows/slash_commands.yml) for commands including

```bash
/stdlib [help check-files update-copyright-years 
  lint-autofix merge rebase]
```

### Native add-ons

```bash
make install-node-addons NODE_ADDONS_PATTERN="lapack/base/zlacgv"
```

### Linting

To run the editorconfig linter:

```bash
make lint-editorconfig-files FILES="lib/node_modules/@stdlib/blas/base/dscal/src/dscal.f"
```
