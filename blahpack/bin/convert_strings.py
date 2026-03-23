#!/usr/bin/env python3
"""
Convert single-char Fortran-style string parameters to full-word stdlib-js
convention across all base.js and test.js files.

Context-aware: uses variable names (uplo, trans, side, diag, etc.) and
function signatures to determine the correct mapping.

Usage:
    python3 bin/convert_strings.py --dry-run    # Show changes without writing
    python3 bin/convert_strings.py              # Apply changes in-place
    python3 bin/convert_strings.py --stats      # Show statistics only
"""

import re
import os
import sys
import glob
import json
from collections import defaultdict

# ─── Mappings ───────────────────────────────────────────────────────────────

# Variable-name → char → full string
PARAM_MAP = {
    'uplo': {
        'U': 'upper', 'u': 'upper',
        'L': 'lower', 'l': 'lower',
        'A': 'all',   'a': 'all',     # used by dlacpy/zlacpy
    },
    'trans': {
        'N': 'no-transpose',        'n': 'no-transpose',
        'T': 'transpose',           't': 'transpose',
        'C': 'conjugate-transpose', 'c': 'conjugate-transpose',
    },
    'transa': {
        'N': 'no-transpose',        'n': 'no-transpose',
        'T': 'transpose',           't': 'transpose',
        'C': 'conjugate-transpose', 'c': 'conjugate-transpose',
    },
    'transb': {
        'N': 'no-transpose',        'n': 'no-transpose',
        'T': 'transpose',           't': 'transpose',
        'C': 'conjugate-transpose', 'c': 'conjugate-transpose',
    },
    'transt': {
        'N': 'no-transpose',        'n': 'no-transpose',
        'T': 'transpose',           't': 'transpose',
        'C': 'conjugate-transpose', 'c': 'conjugate-transpose',
    },
    'side': {
        'L': 'left',  'l': 'left',
        'R': 'right', 'r': 'right',
    },
    'diag': {
        'U': 'unit',     'u': 'unit',
        'N': 'non-unit', 'n': 'non-unit',
    },
    'direct': {
        'F': 'forward',  'f': 'forward',
        'B': 'backward', 'b': 'backward',
    },
    'storev': {
        'C': 'columnwise', 'c': 'columnwise',
        'R': 'rowwise',    'r': 'rowwise',
    },
    'job': {
        'N': 'none',      'n': 'none',
        'P': 'permute',   'p': 'permute',
        'S': 'scale',     's': 'scale',
        'B': 'both',      'b': 'both',
    },
    'compq': {
        'N': 'none', 'n': 'none',
        'I': 'initialize', 'i': 'initialize',
        'V': 'update', 'v': 'update',
    },
    'compz': {
        'N': 'none', 'n': 'none',
        'I': 'initialize', 'i': 'initialize',
        'V': 'update', 'v': 'update',
    },
    'vect': {
        'Q': 'Q', 'q': 'Q',
        'P': 'P', 'p': 'P',
    },
    'jobz': {
        'N': 'none',    'n': 'none',
        'V': 'compute', 'v': 'compute',
    },
    'jobu': {
        'A': 'all-columns', 'a': 'all-columns',
        'S': 'economy',     's': 'economy',
        'O': 'overwrite',   'o': 'overwrite',
        'N': 'none',        'n': 'none',
    },
    'jobvt': {
        'A': 'all-rows',  'a': 'all-rows',
        'S': 'economy',   's': 'economy',
        'O': 'overwrite',  'o': 'overwrite',
        'N': 'none',       'n': 'none',
    },
    'jobvl': {
        'N': 'none',    'n': 'none',
        'V': 'compute', 'v': 'compute',
    },
    'jobvr': {
        'N': 'none',    'n': 'none',
        'V': 'compute', 'v': 'compute',
    },
    'howmny': {
        'A': 'all',      'a': 'all',
        'B': 'backtransform', 'b': 'backtransform',
        'S': 'selected', 's': 'selected',
    },
    'type': {  # zlascl/dlascl matrix type
        'G': 'general',       'g': 'general',
        'L': 'lower',         'l': 'lower',
        'U': 'upper',         'u': 'upper',
        'H': 'upper-hessenberg', 'h': 'upper-hessenberg',
        'B': 'lower-band',    'b': 'lower-band',
        'Q': 'upper-band',    'q': 'upper-band',
        'Z': 'band',          'z': 'band',
    },
    'sort': {
        'I': 'increasing', 'i': 'increasing',
        'D': 'decreasing', 'd': 'decreasing',
    },
}

def auto_detect_string_params():
    """Scan all base.js files to auto-detect string parameters from JSDoc."""
    import glob as g
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    result = {}

    for f in sorted(g.glob(os.path.join(base_dir, 'lib/**/lib/base.js'), recursive=True)):
        with open(f) as fh:
            content = fh.read()

        # Find the MAIN exported function — look at module.exports
        export_match = re.search(r'module\.exports\s*=\s*(\w+)', content)
        if not export_match:
            continue
        func_name = export_match.group(1)

        # Find that function's declaration
        func_pattern = re.compile(
            r'function\s+' + re.escape(func_name) + r'\s*\(([^)]*)\)'
        )
        func_match = func_pattern.search(content)
        if not func_match:
            continue

        params = [p.strip().split(' ')[-1] for p in func_match.group(1).split(',')]

        # Find @param {string} tags in the JSDoc block ABOVE this function
        # Look backwards from the function declaration for the JSDoc block
        func_start = func_match.start()
        jsdoc_end = content.rfind('*/', 0, func_start)
        if jsdoc_end < 0:
            continue
        jsdoc_start = content.rfind('/**', 0, jsdoc_end)
        if jsdoc_start < 0:
            continue
        jsdoc = content[jsdoc_start:jsdoc_end + 2]

        string_params = []
        for m in re.finditer(r'@param\s*\{string\}\s+(\w+)', jsdoc):
            pname = m.group(1)
            if pname in params:
                string_params.append(pname)

        if string_params:
            result[func_name] = string_params

    return result


ROUTINE_STRING_PARAMS = auto_detect_string_params()

# Add norm param mapping
PARAM_MAP['norm'] = {
    'M': 'max',       'm': 'max',
    '1': 'one-norm',
    'O': 'one-norm',  'o': 'one-norm',
    'I': 'inf-norm',  'i': 'inf-norm',
    'F': 'frobenius', 'f': 'frobenius',
    'E': 'frobenius', 'e': 'frobenius',
}

PARAM_MAP['pivot'] = {
    'V': 'variable', 'v': 'variable',
    'T': 'top',      't': 'top',
    'B': 'bottom',   'b': 'bottom',
}

# ─── Patterns ───────────────────────────────────────────────────────────────

# Pattern 1: Variable comparisons like `uplo === 'U'` or `uplo === 'u'`
# Matches: varname === 'X' or varname !== 'X'
COMPARE_PATTERN = re.compile(
    r"""(\b(?:""" + '|'.join(PARAM_MAP.keys()) + r""")\b)\s*(===|!==)\s*'([A-Za-z0-9])'"""
)

# Pattern 2: Compound comparisons like `uplo === 'U' || uplo === 'u'`
COMPOUND_PATTERN = re.compile(
    r"""(\b(?:""" + '|'.join(PARAM_MAP.keys()) + r""")\b)\s*===\s*'([A-Za-z])'\s*\|\|\s*\1\s*===\s*'([A-Za-z])'"""
)

# Pattern 3: Assignment like `transt = 'T'` where variable name indicates context
ASSIGN_PATTERN = re.compile(
    r"""(\b(?:""" + '|'.join(PARAM_MAP.keys()) + r""")\b)\s*=\s*'([A-Za-z])'"""
)


def lookup(varname, char):
    """Look up the full string for a variable name and character."""
    varname_lower = varname.lower()
    if varname_lower in PARAM_MAP:
        mapping = PARAM_MAP[varname_lower]
        if char in mapping:
            return mapping[char]
    return None


def convert_comparisons(content, filepath):
    """Convert comparison patterns in a file's content."""
    changes = []

    # First pass: replace compound patterns `var === 'X' || var === 'x'`
    def replace_compound(m):
        varname = m.group(1)
        upper = m.group(2)
        lower = m.group(3)
        full = lookup(varname, upper)
        if full is None:
            full = lookup(varname, lower)
        if full is None:
            return m.group(0)  # no change
        changes.append((filepath, varname, upper, full))
        return f"{varname} === '{full}'"

    content = COMPOUND_PATTERN.sub(replace_compound, content)

    # Second pass: replace remaining single comparisons `var === 'X'`
    def replace_single(m):
        varname = m.group(1)
        op = m.group(2)
        char = m.group(3)
        full = lookup(varname, char)
        if full is None:
            return m.group(0)
        changes.append((filepath, varname, char, full))
        return f"{varname} {op} '{full}'"

    content = COMPARE_PATTERN.sub(replace_single, content)

    # Third pass: replace assignments `varname = 'X'`
    def replace_assign(m):
        varname = m.group(1)
        char = m.group(2)
        full = lookup(varname, char)
        if full is None:
            return m.group(0)
        changes.append((filepath, f'{varname}=', char, full))
        return f"{varname} = '{full}'"

    content = ASSIGN_PATTERN.sub(replace_assign, content)

    return content, changes


def parse_call_args(content, start_pos):
    """Parse function call arguments starting after the opening paren.
    Returns list of (arg_start, arg_end) positions for each argument."""
    args = []
    paren_depth = 1
    pos = start_pos
    arg_start = start_pos
    in_string = False
    string_char = None

    while pos < len(content) and paren_depth > 0:
        c = content[pos]
        if in_string:
            if c == '\\':
                pos += 1  # skip escaped char
            elif c == string_char:
                in_string = False
        elif c == "'" or c == '"':
            in_string = True
            string_char = c
        elif c == '(':
            paren_depth += 1
        elif c == ')':
            paren_depth -= 1
            if paren_depth == 0:
                args.append((arg_start, pos))
                break
        elif c == ',' and paren_depth == 1:
            args.append((arg_start, pos))
            arg_start = pos + 1
        pos += 1

    return args


def convert_call_literals(content, filepath):
    """Convert string literals in function call arguments."""
    changes = []
    replacements = []  # (start, end, old, new) — apply in reverse order

    for routine, param_names in ROUTINE_STRING_PARAMS.items():
        if not param_names:
            continue

        # Match routine(, routine.ndarray(, routinebase(, routineBase(
        call_pattern = re.compile(
            r'\b' + re.escape(routine) + r'(?:[Bb]ase|\.ndarray)?\s*\('
        )

        for m in call_pattern.finditer(content):
            args = parse_call_args(content, m.end())

            # The string params are at the BEGINNING of the arg list
            for i, pname in enumerate(param_names):
                if i >= len(args):
                    break
                arg_start, arg_end = args[i]
                arg_text = content[arg_start:arg_end].strip()

                # Check if this argument is a single-char string literal
                single_char = re.match(r"^'([A-Za-z0-9])'$", arg_text)
                if single_char:
                    char = single_char.group(1)
                    full = lookup(pname, char)
                    if full and full != char:
                        # Find the exact position of the string literal
                        lit_match = re.search(r"'[A-Za-z0-9]'", content[arg_start:arg_end])
                        if lit_match:
                            abs_start = arg_start + lit_match.start()
                            abs_end = arg_start + lit_match.end()
                            replacements.append((abs_start, abs_end, f"'{char}'", f"'{full}'"))
                            changes.append((filepath, f'{routine}({pname})', char, full))

    # Apply replacements in reverse order to preserve positions
    replacements.sort(key=lambda x: x[0], reverse=True)
    for start, end, old, new in replacements:
        content = content[:start] + new + content[end:]

    return content, changes


def convert_file(filepath, dry_run=False):
    """Convert a single file."""
    with open(filepath, 'r') as f:
        original = f.read()

    content = original
    all_changes = []

    # Convert comparisons (variable name gives context)
    content, changes = convert_comparisons(content, filepath)
    all_changes.extend(changes)

    # Convert function call arguments (position gives context)
    content, changes = convert_call_literals(content, filepath)
    all_changes.extend(changes)

    # Convert generic calls (base('N', ...) etc. using file path context)
    content, changes = convert_generic_calls(content, filepath)
    all_changes.extend(changes)

    # Convert object literal params in test files ({ s: 'L', u: 'U' })
    if '/test/' in filepath:
        content, changes = convert_test_object_literals(content, filepath)
        all_changes.extend(changes)

    if content != original and not dry_run:
        with open(filepath, 'w') as f:
            f.write(content)

    return all_changes, content != original


def determine_routine_for_file(filepath):
    """Determine which routine a file belongs to from its path."""
    # Extract routine name from path like lib/blas/base/dgemm/lib/base.js
    # or lib/lapack/base/dgetrf/test/test.js
    parts = filepath.replace('\\', '/').split('/')
    for i, p in enumerate(parts):
        if p == 'base' and i + 1 < len(parts) and i > 0:
            if parts[i - 1] in ('blas', 'lapack'):
                return parts[i + 1]
    return None


def convert_test_object_literals(content, filepath):
    """Convert object-literal string params like { s: 'L', u: 'U', t: 'N', d: 'N' }."""
    changes = []

    # Map short keys to param types
    key_map = {
        's': 'side', 'u': 'uplo', 't': 'trans', 'd': 'diag',
    }

    # Pattern: key: 'X' inside object literals
    for short_key, param_type in key_map.items():
        pattern = re.compile(
            r"(\b" + re.escape(short_key) + r")\s*:\s*'([A-Za-z])'"
        )
        def make_replacer(pt):
            def replacer(m):
                key = m.group(1)
                char = m.group(2)
                full = lookup(pt, char)
                if full:
                    changes.append((filepath, f'obj.{key}', char, full))
                    return f"{key}: '{full}'"
                return m.group(0)
            return replacer
        content = pattern.sub(make_replacer(param_type), content)

    return content, changes


def convert_generic_calls(content, filepath):
    """Convert calls like base('N', ...) where we know the routine from the file path."""
    routine = determine_routine_for_file(filepath)
    if not routine or routine not in ROUTINE_STRING_PARAMS:
        return content, []

    param_names = ROUTINE_STRING_PARAMS[routine]
    if not param_names:
        return content, []

    changes = []
    replacements = []

    # Match any call where first args are single-char strings:
    # base(, <routine>(, <routine>Base(, <routine>base(, <routine>.ndarray(
    # Also match common aliases: fn(, func(, result(
    call_pattern = re.compile(r'\b(\w+(?:\.\w+)?)\s*\(')

    for m in call_pattern.finditer(content):
        callee = m.group(1)
        # Skip known non-routine calls
        if callee in ('require', 'test', 'it', 'describe', 'assert', 'function',
                       'Array', 'Float64Array', 'Int32Array', 'Complex128Array',
                       'Complex128', 'console', 'Math', 'Number', 'Object',
                       'findCase', 'assertArrayClose', 'assertClose',
                       'setupV4x2', 'setupT2x2', 'setupC4x3', 'new',
                       'reinterpret', 'real', 'imag', 'from', 'push',
                       'setTimeout', 'setInterval', 'Error', 'TypeError',
                       'RangeError', 'JSON', 'format', 'join', 'slice',
                       'toString', 'indexOf', 'splice', 'concat', 'map',
                       'filter', 'reduce', 'forEach', 'isNaN', 'parseFloat',
                       'parseInt', 'String', 'Boolean', 'RegExp', 'Date',
                       'Promise', 'Symbol', 'Set', 'Map', 'WeakMap',
                       'WeakSet', 'Proxy', 'Reflect', 'ArrayBuffer',
                       'DataView', 'SharedArrayBuffer', 'Atomics',
                       'BigInt', 'BigInt64Array', 'BigUint64Array',
                       'process', 'Buffer', 'path', 'fs', 'module',
                       'exports', 'global', 'window', 'document',
                       'equal', 'deepEqual', 'strictEqual', 'ok',
                       'throws', 'doesNotThrow', 'notEqual',
                       'notStrictEqual', 'notDeepEqual', 'match',
                       'doesNotMatch', 'ifError', 'fail',
                       'setReadOnly', 'tryRequire', 'isError',
                       'tape', 'bench', 'benchmark'):
            continue

        args = parse_call_args(content, m.end())
        if len(args) < len(param_names):
            continue

        # Check if the first len(param_names) args are single-char strings
        all_match = True
        for i, pname in enumerate(param_names):
            arg_text = content[args[i][0]:args[i][1]].strip()
            if not re.match(r"^'[A-Za-z0-9]'$", arg_text):
                all_match = False
                break

        if not all_match:
            continue

        # All leading args are single-char strings matching the expected count
        for i, pname in enumerate(param_names):
            arg_start, arg_end = args[i]
            arg_text = content[arg_start:arg_end].strip()
            char = arg_text[1]  # extract char from 'X'
            full = lookup(pname, char)
            if full and full != char:
                lit_match = re.search(r"'[A-Za-z0-9]'", content[arg_start:arg_end])
                if lit_match:
                    abs_start = arg_start + lit_match.start()
                    abs_end = arg_start + lit_match.end()
                    replacements.append((abs_start, abs_end, f"'{char}'", f"'{full}'"))
                    changes.append((filepath, f'{callee}({pname})', char, full))

    # Apply replacements in reverse
    replacements.sort(key=lambda x: x[0], reverse=True)
    for start, end, old, new in replacements:
        content = content[:start] + new + content[end:]

    return content, changes


def find_files():
    """Find all base.js and test.js files."""
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    files = []
    for pattern in ['lib/**/base.js', 'lib/**/test/test*.js']:
        files.extend(glob.glob(os.path.join(base_dir, pattern), recursive=True))
    return sorted(files)


def main():
    dry_run = '--dry-run' in sys.argv
    stats_only = '--stats' in sys.argv

    files = find_files()
    total_changes = 0
    total_files_changed = 0
    change_counts = defaultdict(int)

    for filepath in files:
        rel = os.path.relpath(filepath)
        changes, modified = convert_file(filepath, dry_run=dry_run or stats_only)
        if changes:
            total_files_changed += 1
            total_changes += len(changes)
            for _, varname, char, full in changes:
                change_counts[f"{varname}: '{char}' → '{full}'"] += 1
            if not stats_only:
                print(f"{'[DRY RUN] ' if dry_run else ''}Modified: {rel} ({len(changes)} changes)")
                if dry_run:
                    for _, varname, char, full in changes[:5]:
                        print(f"    {varname}: '{char}' → '{full}'")
                    if len(changes) > 5:
                        print(f"    ... and {len(changes) - 5} more")

    print(f"\n{'=' * 60}")
    print(f"Total: {total_changes} changes across {total_files_changed} files")
    print(f"\nBreakdown:")
    for key, count in sorted(change_counts.items(), key=lambda x: -x[1]):
        print(f"  {count:4d}x  {key}")

    if dry_run:
        print(f"\nThis was a dry run. Run without --dry-run to apply changes.")


if __name__ == '__main__':
    main()
