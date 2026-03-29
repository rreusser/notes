---
name: blahpack-lint
description: Run ESLint with stdlib rules, fork rules for customization, or create new fixable rules
argument-hint: [path | --fix path | --fork <rule-name> | --new <rule-name>]
---

# ESLint Setup

Blahpack uses a **copy-on-write** ESLint setup. All 122 stdlib rules load from
the stdlib checkout by default. When a rule needs modification, it gets forked
locally into `tools/eslint/rules/` where it takes priority.

## Lint files

```bash
# Lint specific files or directories:
bin/lint.sh lib/blas/base/daxpy/lib/base.js
bin/lint.sh lib/lapack/base/dpotf2/lib/

# Lint everything:
bin/lint.sh
npm run lint

# Auto-fix:
bin/lint.sh --fix lib/
npm run lint:fix
```

If the user provides `$ARGUMENTS`, lint those paths. Otherwise lint the path
the user is working on or default to `lib/`.

## Fork a stdlib rule for customization

```bash
bin/fork-eslint-rule.sh <rule-name>
```

This copies the rule to `tools/eslint/rules/<name>.cjs`, rewrites `@stdlib/*`
imports to `tools/eslint/shims.cjs`, and copies companion files (e.g.
`defaults.json`). The local copy immediately overrides the stdlib version.

If the user asks to fork a rule (`$ARGUMENTS` starts with `--fork`), run the
fork script with the rule name.

## Create a new fixable rule

To create a brand-new rule with `--fix` support:

1. Create `tools/eslint/rules/<name>.cjs` following this template:

```js
'use strict';

var rule = {
    'meta': {
        'docs': {
            'description': 'description of what the rule checks'
        },
        'schema': [],
        'fixable': 'code'  // enables --fix
    },
    'create': function main( context ) {
        return {
            // AST visitor methods
            'CallExpression': function validate( node ) {
                context.report({
                    node: node,
                    message: 'Error message',
                    fix: function( fixer ) {
                        return fixer.replaceText( node, 'replacement' );
                    }
                });
            }
        };
    }
};

module.exports = rule;
```

2. Enable the rule in `tools/eslint/config/blahpack.json`:

```json
{
    "stdlib/<name>": "error"
}
```

If the user asks to create a new rule (`$ARGUMENTS` starts with `--new`),
scaffold the rule file and add the config entry.

## Key files

| File | Purpose |
|------|---------|
| `.eslintrc.cjs` | Root config (merges all severity configs) |
| `tools/eslint/plugin.cjs` | Copy-on-write plugin loader |
| `tools/eslint/shims.cjs` | Drop-in replacements for @stdlib utilities |
| `tools/eslint/find-jsdoc.cjs` | JSDoc comment locator (used by 72+ rules) |
| `tools/eslint/config/*.json` | Rule severity configs (from stdlib + blahpack overrides) |
| `tools/eslint/config/blahpack.json` | Blahpack-specific overrides and custom rule config |
| `tools/eslint/rules/` | Locally forked or new rules (.cjs files) |
| `bin/fork-eslint-rule.sh` | Script to fork a stdlib rule locally |

## Notes

- All `.js` files in this project are ESM (`"type": "module"` in package.json),
  so ESLint tooling files use `.cjs` extension for CommonJS.
- One stdlib rule (`jsdoc-markdown-remark`) is disabled due to a remark version
  incompatibility when loaded cross-project.
- The stdlib plugin resolves via `tools/eslint/node_modules/eslint-plugin-stdlib/`.
- Run with `--resolve-plugins-relative-to tools/eslint` (already wired into
  `bin/lint.sh` and `package.json` scripts).
