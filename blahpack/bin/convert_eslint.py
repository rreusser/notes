#!/usr/bin/env python3
"""Convert inline eslint-disable-line comments to file-level eslint-disable.

Scans all .js files under lib/, collects eslint-disable-line rules per file,
adds a file-level /* eslint-disable RULES */ comment before 'use strict',
and removes the inline comments.

Usage:
    python bin/convert_eslint.py          # dry-run (shows what would change)
    python bin/convert_eslint.py --write  # apply changes
"""

import os
import re
import sys
from pathlib import Path

LIB_DIR = Path(__file__).resolve().parent.parent / 'lib'

# Match inline eslint-disable-line comments
INLINE_RE = re.compile(r'\s*//\s*eslint-disable-line\s+([\w\-,\s]+)')


def process_file(filepath):
    """Process a single JS file. Returns (new_content, rules_set) or None if no changes."""
    with open(filepath, 'r') as f:
        content = f.read()

    if 'eslint-disable-line' not in content:
        return None

    lines = content.split('\n')
    rules = set()
    new_lines = []
    has_file_level = False

    # Check if file already has a file-level eslint-disable
    for line in lines:
        if re.match(r'\s*/\*\s*eslint-disable\s', line):
            has_file_level = True
            break

    for line in lines:
        m = INLINE_RE.search(line)
        if m:
            # Collect the rules
            rule_str = m.group(1).strip()
            for rule in re.split(r'[,\s]+', rule_str):
                rule = rule.strip()
                if rule:
                    rules.add(rule)
            # Remove the inline comment
            cleaned = line[:m.start()]
            # Remove trailing whitespace but keep the line
            cleaned = cleaned.rstrip()
            new_lines.append(cleaned)
        else:
            new_lines.append(line)

    if not rules:
        return None

    # If there's already a file-level disable, merge rules into it
    if has_file_level:
        merged_lines = []
        for line in new_lines:
            m2 = re.match(r'(\s*/\*\s*eslint-disable\s+)([\w\-,\s]+)(\s*\*/)', line)
            if m2:
                existing_rules = set(re.split(r'[,\s]+', m2.group(2).strip()))
                all_rules = sorted(existing_rules | rules)
                merged_lines.append(f'/* eslint-disable {", ".join(all_rules)} */')
            else:
                merged_lines.append(line)
        new_lines = merged_lines
    else:
        # Insert file-level disable before 'use strict'
        inserted = False
        result_lines = []
        for i, line in enumerate(new_lines):
            if not inserted and line.strip() == "'use strict';":
                # Insert eslint-disable before 'use strict'
                sorted_rules = sorted(rules)
                result_lines.append(f'/* eslint-disable {", ".join(sorted_rules)} */')
                result_lines.append('')
                inserted = True
            result_lines.append(line)
        if not inserted:
            # No 'use strict' found — insert at top
            sorted_rules = sorted(rules)
            result_lines = [f'/* eslint-disable {", ".join(sorted_rules)} */', ''] + new_lines
        new_lines = result_lines

    new_content = '\n'.join(new_lines)
    if new_content == content:
        return None

    return new_content, rules


def main():
    write = '--write' in sys.argv

    total_files = 0
    total_rules = set()

    for root, dirs, files in os.walk(LIB_DIR):
        for fname in sorted(files):
            if not fname.endswith('.js'):
                continue
            filepath = os.path.join(root, fname)
            result = process_file(filepath)
            if result is None:
                continue

            new_content, rules = result
            total_files += 1
            total_rules |= rules
            rel = os.path.relpath(filepath, LIB_DIR.parent)

            if write:
                with open(filepath, 'w') as f:
                    f.write(new_content)
                print(f'  Updated: {rel}')
            else:
                print(f'  Would update: {rel} (rules: {", ".join(sorted(rules))})')

    print(f'\n{"Updated" if write else "Would update"} {total_files} files')
    print(f'Rules found: {", ".join(sorted(total_rules))}')


if __name__ == '__main__':
    main()
