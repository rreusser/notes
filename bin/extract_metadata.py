#!/usr/bin/env python
"""
Extract metadata from reference BLAS/LAPACK Fortran source files.

Produces a JSON database grouped by algorithm, with each variant carrying
its own description, signature, and parsed argument list.
"""

import json
import os
import re
import sys
from collections import Counter, defaultdict

BLAS_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'data', 'BLAS-3.12.0'))
LAPACK_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'data', 'lapack-3.12.0', 'SRC'))

TYPE_PREFIXES = {
    's': {'precision': 'single', 'domain': 'real', 'label': 'single-precision real'},
    'd': {'precision': 'double', 'domain': 'real', 'label': 'double-precision real'},
    'c': {'precision': 'single', 'domain': 'complex', 'label': 'single-precision complex'},
    'z': {'precision': 'double', 'domain': 'complex', 'label': 'double-precision complex'},
    'i': {'precision': 'integer', 'domain': 'integer', 'label': 'integer'},
}

STORAGE_CODES = {
    'ge': 'general',
    'gb': 'general band',
    'gt': 'general tridiagonal',
    'po': 'symmetric/Hermitian positive definite',
    'pp': 'symmetric/Hermitian positive definite (packed)',
    'pb': 'symmetric/Hermitian positive definite (band)',
    'pt': 'symmetric/Hermitian positive definite (tridiagonal)',
    'pf': 'symmetric/Hermitian positive definite (rectangular full packed)',
    'sy': 'symmetric',
    'sp': 'symmetric (packed)',
    'sb': 'symmetric (band)',
    'st': 'symmetric tridiagonal',
    'he': 'Hermitian',
    'hp': 'Hermitian (packed)',
    'hb': 'Hermitian (band)',
    'tr': 'triangular',
    'tp': 'triangular (packed)',
    'tb': 'triangular (band)',
    'tf': 'triangular (rectangular full packed)',
    'or': 'orthogonal',
    'op': 'orthogonal (packed)',
    'un': 'unitary',
    'up': 'unitary (packed)',
    'gg': 'general matrices (generalized problem)',
    'hg': 'upper Hessenberg (generalized)',
    'tg': 'triangular (generalized)',
    'tz': 'trapezoidal',
    'ps': 'positive semi-definite',
    'hf': 'Hermitian (rectangular full packed)',
    'sf': 'symmetric (rectangular full packed)',
    'bb': 'block bidiagonal',
    'gs': 'generalized SVD',
    'la': 'auxiliary',
    'hs': 'upper Hessenberg',
    'bd': 'bidiagonal',
    'di': 'diagonal',
}


def strip_doc_prefix(line):
    """Remove Fortran comment/doc prefix (*> or !> or * or !)."""
    line = line.rstrip('\n\r')
    m = re.match(r'^[*!]>\s?', line)
    if m:
        return line[m.end():]
    m = re.match(r'^[*!]\s?', line)
    if m:
        return line[m.end():]
    return line


def parse_fortran_file(filepath):
    """Parse a single Fortran file and extract metadata."""
    with open(filepath, 'r', errors='replace') as f:
        lines = f.readlines()

    if not lines:
        return None

    filename = os.path.basename(filepath)
    name_from_file = os.path.splitext(filename)[0].upper()

    brief = ''
    purpose_lines = []
    signature_lines = []

    # --- Extract brief ---
    first_line = lines[0].rstrip('\n\r')
    m = re.search(r'\\brief\s+<b>\s*(.+?)</b>(.*)', first_line)
    if m:
        brief = (m.group(1).strip() + ' ' + m.group(2).strip()).strip()
    else:
        m = re.search(r'\\brief\s+\\b\s+(\S+)\s+(.*)', first_line)
        if m and m.group(2).strip():
            brief = m.group(2).strip()

    # --- Extract purpose (used to derive description) ---
    in_purpose = False
    in_verbatim = False
    for line in lines:
        stripped = strip_doc_prefix(line)
        if r'\par Purpose:' in line:
            in_purpose = True
            continue
        if in_purpose and r'\verbatim' in line and r'\endverbatim' not in line:
            in_verbatim = True
            continue
        if in_purpose and in_verbatim:
            if r'\endverbatim' in line:
                break
            purpose_lines.append(stripped)

    # --- Extract subroutine/function signature from Definition section ---
    in_definition = False
    collecting_sig = False
    for line in lines:
        raw = line.rstrip('\n\r')
        if re.search(r'Definition:', raw):
            in_definition = True
            continue
        if in_definition:
            sig_match = re.match(
                r'^[*! ]\s{6,}((?:SUBROUTINE|FUNCTION|INTEGER\s+FUNCTION|'
                r'DOUBLE\s+PRECISION\s+FUNCTION|REAL\s+FUNCTION|'
                r'COMPLEX\*16\s+FUNCTION|COMPLEX\s+FUNCTION|'
                r'LOGICAL\s+FUNCTION|CHARACTER\*1\s+FUNCTION)\s+.+)',
                raw
            )
            if sig_match or collecting_sig:
                if sig_match and not collecting_sig:
                    collecting_sig = True
                    signature_lines.append(sig_match.group(1).strip())
                elif collecting_sig:
                    cont = re.match(r'^[*! ]\s+\$\s*(.*)', raw)
                    if cont:
                        signature_lines.append(cont.group(1).strip())
                    else:
                        if signature_lines:
                            break
            if re.search(r'Scalar Arguments|Array Arguments', raw) and not collecting_sig:
                break
            if re.search(r'^\*\s*$', raw) and collecting_sig:
                break

    # --- Extract \param blocks ---
    arguments = []
    i = 0
    while i < len(lines):
        line = lines[i]
        param_match = re.search(r'\\param\[([^\]]+)\]\s+(\S+)', line)
        if param_match:
            direction = param_match.group(1).strip()
            arg_name = param_match.group(2).strip()
            # Collect the verbatim block for this param
            arg_type = ''
            # Find the verbatim content
            i += 1
            in_verb = False
            while i < len(lines):
                pline = strip_doc_prefix(lines[i])
                if r'\verbatim' in lines[i] and r'\endverbatim' not in lines[i]:
                    in_verb = True
                    i += 1
                    continue
                if in_verb:
                    if r'\endverbatim' in lines[i]:
                        break
                    # First non-blank line: "NAME is TYPE [array, dimension (...)]"
                    if not arg_type:
                        stripped = pline.strip()
                        # Parse "NAME is TYPE" or "NAME is TYPE array, dimension (...)"
                        m = re.match(
                            r'\S+\s+is\s+(.+)',
                            stripped,
                            re.IGNORECASE
                        )
                        if m:
                            arg_type = m.group(1).strip()
                i += 1
            arguments.append({
                'name': arg_name,
                'direction': direction,
                'type': arg_type,
            })
        i += 1

    purpose = '\n'.join(purpose_lines).strip()
    signature = ' '.join(signature_lines).strip()
    purpose = re.sub(r' {2,}', ' ', purpose)

    # --- Derive first sentence of purpose as description ---
    desc = ''
    if purpose:
        # Collapse to single line for easier matching
        flat = re.sub(r'\s+', ' ', purpose).strip()
        # Find first sentence-ending period: a period followed by
        # whitespace or end-of-string, but NOT inside parens/pipes.
        # We track nesting depth to avoid matching "." inside |Re(.)|
        best = None
        depth = 0
        for i, ch in enumerate(flat):
            if ch in ('(', '|'):
                depth += 1
            elif ch in (')', ):
                depth = max(0, depth - 1)
            elif ch == '|' and depth > 0:
                depth -= 1
            elif ch == '.' and depth == 0:
                # Check next char is whitespace or end
                if i + 1 >= len(flat) or flat[i + 1] in (' ', '\t', '\n'):
                    best = i
                    break
        if best is not None:
            desc = flat[:best + 1]
        else:
            desc = flat

    # Downcase ALL-CAPS descriptions (some old BLAS routines)
    if desc and desc == desc.upper() and len(desc) > 10:
        desc = desc[0] + desc[1:].lower()

    if brief:
        desc = brief

    return {
        'name': name_from_file,
        'filename': filename,
        'description': desc,
        'signature': signature,
        'arguments': arguments,
    }


# Mixed-type BLAS routines where the name encodes two type prefixes.
# Maps uppercase routine name -> (type, algorithm).
# The "type" is the primary data type the routine operates on.
MIXED_TYPE_OVERRIDES = {
    'ICAMAX':  ('c', 'amax'),   # integer result, complex input
    'IDAMAX':  ('d', 'amax'),
    'ISAMAX':  ('s', 'amax'),
    'IZAMAX':  ('z', 'amax'),
    'SCASUM':  ('c', 'asum'),   # real result, complex input
    'DZASUM':  ('z', 'asum'),
    'SCNRM2':  ('c', 'nrm2'),
    'DZNRM2':  ('z', 'nrm2'),
    'DCABS1':  ('z', 'cabs1'),  # double result, complex*16 input
    'SCABS1':  ('c', 'cabs1'),
    'CSROT':   ('c', 'rot'),    # complex vector, real rotation
    'ZDROT':   ('z', 'rot'),
    'CSSCAL':  ('c', 'scal'),   # complex vector, real scalar
    'ZDSCAL':  ('z', 'scal'),
    'DSDOT':   ('s', 'dot'),    # double result, single input -> still a dot
    'SDSDOT':  ('s', 'dot'),    # single+double accumulator, single input
    'CSRSCL':  ('c', 'rscl'),   # complex vector, real scalar
    'ZDRSCL':  ('z', 'rscl'),
    'ICMAX1':  ('c', 'amax'),   # variant of icamax
    'IZMAX1':  ('z', 'amax'),
    'SCSUM1':  ('c', 'sum1'),
    'DZSUM1':  ('z', 'sum1'),
}


def classify_routine(name):
    """Classify a routine by data type prefix and storage scheme."""
    upper = name.upper()
    lower = name.lower()

    # Check for mixed-type override first
    if upper in MIXED_TYPE_OVERRIDES:
        dtype_prefix, algorithm = MIXED_TYPE_OVERRIDES[upper]
        return {
            'type': dtype_prefix,
            'storage': None,
            'generic_name': lower[1:],
            'algorithm': algorithm,
        }

    prefix_char = lower[0] if lower else ''
    if prefix_char in TYPE_PREFIXES:
        dtype_prefix = prefix_char
    elif prefix_char in ('x', 'l'):
        dtype_prefix = prefix_char
    else:
        dtype_prefix = prefix_char

    storage = None
    rest = lower[1:] if prefix_char in TYPE_PREFIXES or prefix_char in ('x', 'l') else lower

    for code in sorted(STORAGE_CODES, key=len, reverse=True):
        if rest.startswith(code):
            storage = code
            rest = rest[len(code):]
            break

    # For auxiliary (la*) routines, try to extract a trailing storage code
    # from the algorithm suffix. E.g., DLANGE -> la + nge -> algorithm=n,
    # storage=ge. This groups norm routines, factorization helpers, etc.
    # Only do this if stripping the code leaves a recognized algorithm
    # prefix (at least 1 char) and the result makes sense.
    KNOWN_LA_ALGORITHMS = {
        'n',       # matrix norm (lange, lansy, lantr, ...)
        'cond',    # condition number estimate
        'equb',    # equilibration (row/col scaling)
        'equ',     # equilibration
        'swp',     # symmetric matrix swap
        'uum',     # multiply by orthogonal/unitary matrix
        'uu2',     # multiply by orthogonal/unitary (unblocked)
        'con',     # condition number
        'rfs',     # iterative refinement
        'trs',     # triangular solve
        'trd',     # reduction to tridiagonal/bidiagonal
    }
    if storage == 'la' and len(rest) > 2:
        for code in sorted(STORAGE_CODES, key=len, reverse=True):
            if code == 'la':
                continue
            if rest.endswith(code) and rest[:-len(code)] in KNOWN_LA_ALGORITHMS:
                storage = code
                rest = rest[:-len(code)]
                break

    generic = lower[1:] if prefix_char in TYPE_PREFIXES or prefix_char in ('x', 'l') else lower

    return {
        'type': dtype_prefix,
        'storage': storage,
        'generic_name': generic,
        'algorithm': rest,
    }


def strip_routine_name(desc, names):
    """Strip leading routine name from a description string."""
    for name in names:
        if desc.upper().startswith(name + ' '):
            desc = desc[len(name) + 1:]
            break
        if desc.upper().startswith(name + ','):
            desc = desc[len(name) + 1:].lstrip()
            break
    if desc:
        desc = desc[0].upper() + desc[1:]
    return desc


def main():
    routines = []

    sources = []
    import glob as g
    for ext in ('*.f', '*.f90'):
        sources += g.glob(os.path.join(BLAS_DIR, ext))
        sources += g.glob(os.path.join(LAPACK_DIR, ext))

    skip_files = {'blas.pc.in', 'la_constants.f90', 'la_xisnan.f90'}

    for filepath in sorted(sources):
        basename = os.path.basename(filepath)
        if basename in skip_files:
            continue

        meta = parse_fortran_file(filepath)
        if meta is None:
            continue

        classification = classify_routine(meta['name'])

        if BLAS_DIR in os.path.abspath(filepath):
            library = 'BLAS'
        else:
            library = 'LAPACK'

        entry = {
            'name': meta['name'],
            'library': library,
            'algorithm': classification['algorithm'],
            'type': classification['type'],
            'storage': classification['storage'],
            'description': meta['description'],
            'signature': meta['signature'],
            'arguments': meta['arguments'],
        }
        routines.append(entry)

    # Group routines by (library, algorithm suffix)
    groups = defaultdict(list)
    for r in routines:
        groups[(r['library'], r['algorithm'])].append(r)

    grouped = []
    for (library, algorithm) in sorted(groups):
        members = groups[(library, algorithm)]

        # Build variants list with full metadata per variant
        variants = []
        all_names = [r['name'] for r in members]
        for r in sorted(members, key=lambda r: r['name']):
            desc = strip_routine_name(r['description'], all_names)
            variants.append({
                'name': r['name'],
                'type': r['type'],
                'storage': r['storage'],
                'description': desc,
                'signature': r['signature'],
                'arguments': r['arguments'],
            })

        entry = {
            'algorithm': algorithm,
            'library': library,
            'variants': variants,
        }
        grouped.append(entry)

    # Summary statistics
    type_counts = Counter(r['type'] for r in routines)
    storage_counts = Counter(r['storage'] or 'none' for r in routines)

    output = {
        'meta': {
            'blas_version': '3.12.0',
            'lapack_version': '3.12.0',
            'total_routines': len(routines),
            'total_algorithms': len(grouped),
            'by_type_prefix': dict(sorted(type_counts.items())),
            'by_storage_code': dict(sorted(storage_counts.items())),
            'type_prefixes': TYPE_PREFIXES,
            'storage_codes': STORAGE_CODES,
        },
        'routines': grouped,
    }

    json.dump(output, sys.stdout, indent=2)
    print()


if __name__ == '__main__':
    main()
