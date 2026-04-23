#!/usr/bin/env python3
"""
Fix ESLint jsdoc-no-paragraph-content-indent and jsdoc-no-inline-padding
errors in JSDoc comments of base.js files.

Two fixes:
1. paragraph-content-indent: Remove leading spaces from JSDoc continuation
   lines (lines starting with `*   ` inside JSDoc blocks, excluding license
   header URLs and valid markdown list nesting).
2. inline-padding: Replace ` _ X _ ` (underscore emphasis with spaces)
   with `*` multiplication. Also fix lines that start with `_` instead of `*`
   due to emphasis patterns.
"""

import re
import sys
import os


def is_in_license_header(lines, idx):
    """Check if line idx is inside the license header comment block."""
    if idx < 17 and 'http://' in lines[idx]:
        return True
    return False


def is_markdown_list_nesting(line):
    """Check if a line is a valid nested markdown list item.

    Lines like `*     -   text` are nested list items and should keep indentation.
    """
    m = re.match(r'^\* {3,}-\s', line)
    return m is not None


def process_file(filepath):
    """Process a single base.js file to fix JSDoc lint issues."""
    with open(filepath, 'r') as f:
        content = f.read()

    lines = content.split('\n')

    # Pass 1: Fix lines starting with `_` instead of `*` (from emphasis pattern)
    # AND fix inline padding (` _ ` -> `*`)
    result = []
    for i, line in enumerate(lines):
        stripped = line.lstrip()

        # Fix lines starting with `_` that should start with `*`
        # These are JSDoc continuation lines where the `_` was part of emphasis
        if stripped.startswith('_') and i > 17:
            # Replace the first `_` with `*`
            idx = line.index('_')
            line = line[:idx] + '*' + line[idx+1:]

        # Fix inline padding (` _ ` -> `*`) in JSDoc comment lines
        stripped = line.lstrip()
        if stripped.startswith('*') and ' _ ' in line:
            line = line.replace(' _ ', '*')

        result.append(line)

    lines = result

    # Pass 2: Remove JSDoc paragraph content indentation
    result = []
    for i, line in enumerate(lines):
        # Skip license header URL
        if is_in_license_header(lines, i):
            result.append(line)
            continue

        # Skip valid markdown list nesting
        if is_markdown_list_nesting(line):
            result.append(line)
            continue

        # Fix indented JSDoc lines: `*   text` -> `* text`
        m = re.match(r'^(\*) {3,}(.+)$', line)
        if m:
            line = m.group(1) + ' ' + m.group(2)

        result.append(line)

    new_content = '\n'.join(result)

    if new_content != content:
        with open(filepath, 'w') as f:
            f.write(new_content)
        return True
    return False


def main():
    import glob

    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    patterns = [
        os.path.join(base_dir, 'lib/blas/base/*/lib/base.js'),
        os.path.join(base_dir, 'lib/lapack/base/*/lib/base.js'),
    ]

    files = []
    for pattern in patterns:
        files.extend(sorted(glob.glob(pattern)))

    changed = 0
    for filepath in files:
        if process_file(filepath):
            rel = os.path.relpath(filepath, base_dir)
            print(f'Fixed: {rel}')
            changed += 1

    print(f'\n{changed} files modified out of {len(files)} total')


if __name__ == '__main__':
    main()
