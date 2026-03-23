# Bulk Lint Fix Guide

This document describes how to systematically fix ESLint errors across all
`base.js` files using dispatched agents. It captures the process used in the
first bulk-fix session and documents remaining work.

## Session 1 Summary (2026-03-23)

**Starting state:** 9,847 problems (8,995 errors, 852 warnings)
**Ending state:** ~5,519 problems (4,673 errors, 846 warnings) — **44% reduction**

### Rules completed

| Rule | Errors fixed | Notes |
|------|-------------|-------|
| `no-mixed-operators` | 4,276 | Highest risk. One regression found (dlasyf `Math.floor` misparenthesization) caught by dsytrf's 40x40 test. |
| `operator-assignment` | 15 | Mechanical — `x = x + y` → `x += y` |
| `no-lonely-if` | ~35 | `else { if (...) }` → `else if (...)`. Some required large dedents. |

### Rules partially completed

| Rule | Remaining | Notes |
|------|-----------|-------|
| `no-negated-condition` | 19 | `if (!x) { A } else { B }` → `if (x) { B } else { A }`. 5 done, 15 files left. |
| `space-unary-ops` | 28 | 13 files. Not yet started in bulk. |

## Remaining Work

### Current error breakdown (as of session end)

```
2475  function-call-argument-newline    (cosmetic — newline consistency in fn calls)
 676  function-paren-newline            (cosmetic — paren placement)
 619  @cspell/spellchecker              (cosmetic — needs BLAS/LAPACK dictionary)
 396  new-cap                           (cosmetic — Complex128Array etc.)
 143  stdlib/jsdoc-no-paragraph-content-indent  (jsdoc formatting)
 121  stdlib/jsdoc-no-inline-padding    (jsdoc formatting)
 112  max-statements-per-line           (style)
 100  indent                            (introduced by no-lonely-if dedents — fix these first!)
  96  max-len                           (needs eslint-disable)
  93  max-depth                         (needs eslint-disable)
  75  camelcase                         (cosmetic — Fortran-style sa1, sa2)
  74  stdlib/require-globals            (needs `var Float64Array = require(...)` etc.)
  66  no-unused-vars                    (real — investigate each)
  58  stdlib/jsdoc-no-undefined-references
  51  max-statements                    (needs eslint-disable)
  44  stdlib/jsdoc-no-shortcut-reference-link
  34  max-lines-per-function            (needs eslint-disable)
  28  space-unary-ops                   (fixable — add space after `typeof`, `void`, etc.)
  19  no-negated-condition              (fixable — swap if/else branches)
  19  vars-on-top                       (fixable — move var decls to top of function)
  18  max-lines                         (needs eslint-disable)
  17  valid-jsdoc                       (jsdoc formatting)
  16  no-inner-declarations             (fixable — move function decls out of blocks)
  12  stdlib/empty-line-before-comment  (fixable — add blank line before comments)
   8  no-use-before-define              (investigate)
   6  no-restricted-syntax              (investigate)
   5  no-plusplus                        (use `+= 1` instead of `++`)
```

### Priority order for next session

1. **`indent` (100)** — These were introduced by `no-lonely-if` fixes that
   dedented blocks. Fix immediately since they indicate the dedent may have
   been incomplete. Dispatch per-file agents.

2. **`space-unary-ops` (28)** — Mechanical: add space after unary keyword
   operators like `typeof`. Safe, 13 files.

3. **`no-negated-condition` (19)** — Swap if/else branches. Safe, 15 files.

4. **`no-inner-declarations` (16)** — Move function declarations out of
   if/else blocks to the top of the enclosing function.

5. **`vars-on-top` (19)** — Move `var` declarations to top of function scope.

6. **`stdlib/empty-line-before-comment` (12)** — Add blank line before
   comment blocks.

7. **`no-unused-vars` (66)** — Requires investigation. Some may be
   intentional (workspace parameters), others may be dead code.

8. **`stdlib/require-globals` (74)** — Add `var Float64Array = require(...)`
   etc. Mechanical but needs the right require path.

9. **`function-call-argument-newline` (2475) + `function-paren-newline` (676)**
   — The biggest bucket. These require reformatting multi-line function calls
   so all arguments are either on one line or each on its own line. High
   volume but low risk. Consider writing a codemod.

10. **Rules needing `eslint-disable`** — `max-depth`, `max-statements`,
    `max-len`, `max-lines-per-function`, `max-lines`, `max-params`. Add
    per-file disable comments at top of files that exceed limits.

11. **JSDoc rules** — Various jsdoc formatting issues. Lower priority.

12. **`new-cap` (396), `camelcase` (75), `@cspell/spellchecker` (619)** —
    Lowest priority. These relate to naming conventions (Complex128Array,
    Fortran-style variable names) and BLAS/LAPACK terminology that the
    spellchecker doesn't know.

## How to Dispatch Agents

### General approach

1. **Get per-file list for the target rule:**
   ```bash
   bin/lint.sh lib/blas/base/*/lib/base.js lib/lapack/base/*/lib/base.js 2>&1 \
     | awk '/^\//{file=$1} /RULE_NAME/{print file}' \
     | sed 's|/Users/rreusser/gh/rreusser/notes/blahpack/||' | sort -u
   ```

2. **Dispatch 5-7 agents in parallel**, one per file. Each agent:
   - Lints the file (filtered to the target rule)
   - Reads and fixes the errors
   - Re-lints to verify
   - Runs the module's tests
   - Reports results

3. **After each batch completes**, run the full test suite:
   ```bash
   node --test 'lib/**/test/test*.js' 'lib/*.test.js'
   ```

4. **Periodically** (every 3-4 batches), do a full lint count to track progress.

### Agent prompt template

```
Fix ESLint `RULE_NAME` errors in /Users/rreusser/gh/rreusser/notes/blahpack/FILE_PATH

Process:
1. Run: `cd /Users/rreusser/gh/rreusser/notes/blahpack && bin/lint.sh FILE_PATH 2>&1 | grep RULE_NAME`
2. Read the file around those lines and fix each error.
   [RULE-SPECIFIC FIX INSTRUCTIONS]
3. Re-lint to verify: `bin/lint.sh FILE_PATH 2>&1 | grep RULE_NAME`
4. Run tests: `node --test TEST_PATH`
5. Report changes and test results.

CRITICAL: Only fix RULE_NAME errors. Do NOT fix other lint rules.
```

### Rule-specific fix instructions

**`no-mixed-operators`**: Add parentheses around multiplication/division
subexpressions within addition/subtraction. NEVER rearrange or change the
computation. Example: `a + b * c` → `a + (b * c)`.

**`operator-assignment`**: Replace `x = x OP expr` with `x OP= expr`.

**`no-lonely-if`**: Replace `else { if (...) { ... } }` with
`else if (...) { ... }`. Dedent the body by one level.

**`no-negated-condition`**: Replace `if (!cond) { A } else { B }` with
`if (cond) { B } else { A }`.

**`space-unary-ops`**: Add space after unary word operators (`typeof x`,
`void 0`, etc.) and before/after unary operators as needed.

**`indent`**: Fix indentation to use tabs, matching surrounding code.

**`no-inner-declarations`**: Move function declarations out of if/else
blocks to the enclosing function scope (or convert to function expressions).

**`vars-on-top`**: Move `var` declarations to the top of the enclosing
function, before any other statements.

**`stdlib/empty-line-before-comment`**: Add a blank line before comment
lines that immediately follow a code line.

**`function-call-argument-newline` + `function-paren-newline`**: Either
put all arguments on one line, or put each argument on its own line. The
opening paren should be on the same line as the function name, and the
closing paren should be on its own line if arguments are multi-line.

### Batching strategy

- **Small rules (< 20 files)**: Dispatch all files in 2-3 batches.
- **Medium rules (20-50 files)**: Group by error count (fewest first).
- **Large rules (> 100 files)**: Consider writing a codemod in
  `bin/transform.py` first. If the fix is truly mechanical (like
  `no-mixed-operators`), the per-file agent approach scales fine but
  takes many batches.

### Safety rules

- **Always run tests after each agent batch.** The `no-mixed-operators`
  session found 1 regression in ~160 files — the full test suite caught it.
- **Run a full test suite checkpoint every 3-4 batches.**
- **For arithmetic rules** (`no-mixed-operators`), parenthesization can
  change semantics. The dlasyf bug (wrapping `Math.floor(k/nb) * nb` as
  `Math.floor((k/nb) * nb)`) shows this is a real risk.
- **For control flow rules** (`no-lonely-if`, `no-negated-condition`),
  verify indentation is correct after restructuring. The `indent` errors
  (100) introduced in this session are from incomplete dedents.
- **Never use `--fix`** — ESLint's auto-fixer has been observed to change
  arithmetic semantics.

## Lint command reference

```bash
# Lint specific file
bin/lint.sh lib/blas/base/daxpy/lib/base.js

# Lint a module
bin/lint.sh lib/blas/base/daxpy/lib/

# Lint all base.js files
bin/lint.sh lib/blas/base/*/lib/base.js lib/lapack/base/*/lib/base.js

# Count errors by rule
bin/lint.sh lib/blas/base/*/lib/base.js lib/lapack/base/*/lib/base.js 2>&1 \
  | grep -oP '  \S+$' | sort | uniq -c | sort -rn

# List files with a specific rule
bin/lint.sh lib/blas/base/*/lib/base.js lib/lapack/base/*/lib/base.js 2>&1 \
  | awk '/^\//{file=$1} /RULE/{print file}' \
  | sed 's|/Users/rreusser/gh/rreusser/notes/blahpack/||' | sort -u

# Run all tests
node --test 'lib/**/test/test*.js' 'lib/*.test.js'
```
