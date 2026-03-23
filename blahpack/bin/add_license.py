#!/usr/bin/env python
"""
Add Apache-2.0 license headers to JS files in lib/blas/base and lib/lapack/base.

Usage:
    python bin/add_license.py             # dry-run: list files that would be modified
    python bin/add_license.py --write     # actually modify files
"""

import argparse
import glob
import os
import sys

LICENSE_HEADER = """\
/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
"""


def needs_license(path):
    """Check if a file is missing the license header in the first 200 bytes."""
    try:
        with open(path, 'r', errors='replace') as f:
            head = f.read(200)
        return '@license Apache-2.0' not in head
    except FileNotFoundError:
        return False


def add_license(path, write=False):
    """Prepend the license header to the file if missing.

    Returns True if the file was (or would be) modified.
    """
    if not needs_license(path):
        return False

    with open(path, 'r') as f:
        content = f.read()

    # Strip leading blank lines before inserting the header
    stripped = content.lstrip('\n')

    # Build new content: license header + blank line + existing content
    new_content = LICENSE_HEADER + '\n' + stripped

    if write:
        with open(path, 'w') as f:
            f.write(new_content)

    return True


def main():
    parser = argparse.ArgumentParser(
        description='Add Apache-2.0 license headers to JS module files'
    )
    parser.add_argument(
        '--write', action='store_true',
        help='Actually modify files (default is dry-run)'
    )
    args = parser.parse_args()

    # Collect all JS files under lib/blas/base/*/lib/ and lib/lapack/base/*/lib/
    patterns = [
        'lib/blas/base/*/lib/*.js',
        'lib/lapack/base/*/lib/*.js',
    ]

    files = []
    for pattern in patterns:
        files.extend(sorted(glob.glob(pattern)))

    modified = 0
    skipped = 0
    for path in files:
        if add_license(path, write=args.write):
            modified += 1
            action = 'Fixed' if args.write else 'Would fix'
            print(f'  {action}: {path}')
        else:
            skipped += 1

    mode = 'Modified' if args.write else 'Would modify'
    print(f'\n{mode}: {modified} files  |  Already OK: {skipped} files')

    if not args.write and modified > 0:
        print('\nRun with --write to apply changes.')


if __name__ == '__main__':
    main()
