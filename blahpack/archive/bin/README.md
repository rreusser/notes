# Archived Scripts

These are one-time migration scripts whose work has already been applied across the codebase.
They are kept for reference but are no longer part of the active pipeline.

| Script | Purpose | Status |
|--------|---------|--------|
| `add_license.py` | Add Apache-2.0 license headers | Applied to all files |
| `convert_strings.py` | Convert single-char Fortran flags to long-form | Applied codebase-wide |
| `convert_eslint.py` | Convert ESLint config format | Applied |
| `fix_jsdoc_lint.py` | Fix JSDoc formatting | Now caught by ESLint rules |
| `fix_bad_fortran_formatting.py` | Fortran reformatting | Subsumed by `bin/transform.py` |
| `gen_learnings.py` | Generate LEARNINGS.md templates | `bin/scaffold.py` handles new modules |
| `jats_to_markdown.py` | Convert JATS XML references | Unrelated to BLAS/LAPACK pipeline |
