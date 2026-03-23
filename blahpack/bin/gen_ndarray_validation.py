#!/usr/bin/env python3
"""
Generate ndarray.js validation wrappers from base.js signatures.

Reads each module's base.js to determine parameter types and roles, then
generates a proper ndarray.js with:
  - String parameter validation (TypeError)
  - Dimension validation (RangeError)
  - Early return conditions
  - Proper JSDoc with @throws/@example

Usage:
    python bin/gen_ndarray_validation.py                 # dry-run (shows diff summary)
    python bin/gen_ndarray_validation.py --write         # apply changes
    python bin/gen_ndarray_validation.py --preview NAME  # show generated code for one routine
"""

import argparse
import os
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

# ─── String parameter validators ──────────────────────────────────────────

# Parameters with stdlib assertion helpers
STDLIB_VALIDATORS = {
    'trans':   ('isMatrixTranspose', '@stdlib/blas/base/assert/is-transpose-operation', 'a valid transpose operation'),
    'transa':  ('isMatrixTranspose', '@stdlib/blas/base/assert/is-transpose-operation', 'a valid transpose operation'),
    'transb':  ('isMatrixTranspose', '@stdlib/blas/base/assert/is-transpose-operation', 'a valid transpose operation'),
    'transt':  ('isMatrixTranspose', '@stdlib/blas/base/assert/is-transpose-operation', 'a valid transpose operation'),
    'uplo':    ('isMatrixTriangle',  '@stdlib/blas/base/assert/is-matrix-triangle',     'a valid matrix triangle'),
    'side':    ('isOperationSide',   '@stdlib/blas/base/assert/is-operation-side',       'a valid operation side'),
    'diag':    ('isDiagonalType',    '@stdlib/blas/base/assert/is-diagonal-type',        'a valid diagonal type'),
}

# Routines where uplo also accepts 'all' — use manual validation instead
UPLO_ACCEPTS_ALL = {'dlacpy', 'zlacpy', 'dlaset', 'zlaset'}

# Parameters needing manual whitelist validation
MANUAL_VALIDATORS = {
    'norm':    (['one-norm', 'inf-norm', 'max', 'frobenius'], 'a valid norm type'),
    'direct':  (['forward', 'backward'], 'a valid direction'),
    'storev':  (['column-wise', 'row-wise'], 'a valid storage direction'),
    'vect':    (['q', 'p'], 'a valid vector type'),
    'compq':   (['none', 'initialize', 'compute'], 'a valid computation flag'),
    'compz':   (['none', 'initialize', 'compute'], 'a valid computation flag'),
    'job':     (['none', 'permute', 'scale', 'both'], 'a valid job type'),
    'jobz':    (['none', 'compute'], 'a valid job type'),
    'jobu':    (['all', 'some', 'overwrite', 'none'], 'a valid job type'),
    'jobvt':   (['all', 'some', 'overwrite', 'none'], 'a valid job type'),
    'jobvl':   (['compute', 'none'], 'a valid job type'),
    'jobvr':   (['compute', 'none'], 'a valid job type'),
    'type':    (['general', 'lower', 'upper', 'hessenberg', 'band-lower', 'band-upper'], 'a valid matrix type'),
    'pivot':   (['variable', 'top', 'bottom'], 'a valid pivot type'),
    'howmny':  (['all', 'backtransform', 'select'], 'a valid selection type'),
    'normin':  (['yes', 'no'], 'a valid norm-in flag'),
    'id':      (['increasing', 'decreasing'], 'a valid sort order'),
    'range':   (['all', 'value', 'index'], 'a valid range type'),
    'sort':    (['increasing', 'decreasing', 'none'], 'a valid sort type'),
}

# Dimension parameters that should be checked >= 0
DIMENSION_PARAMS = {'M', 'N', 'K', 'nb', 'nrhs'}

# Parameters that are dimensions but should NOT be checked (valid range depends on context)
SKIP_DIMENSION_CHECK = {'ilo', 'ihi', 'k1', 'k2', 'offset', 'lwork', 'kb'}

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
*/"""


# ─── Base.js parsing ─────────────────────────────────────────────────────

def parse_base_js(filepath):
    """Parse base.js to extract function name, params, types, description."""
    with open(filepath) as f:
        content = f.read()

    # Find exported function name
    export_match = re.search(r'module\.exports\s*=\s*(\w+)', content)
    if not export_match:
        return None
    func_name = export_match.group(1)

    # Find function declaration
    func_re = re.compile(r'function\s+' + re.escape(func_name) + r'\s*\(([^)]*)\)')
    func_match = func_re.search(content)
    if not func_match:
        return None

    raw_params = [p.strip() for p in func_match.group(1).split(',') if p.strip()]

    # Parse @param JSDoc
    func_start = func_match.start()
    jsdoc_end = content.rfind('*/', 0, func_start)
    if jsdoc_end < 0:
        return None
    jsdoc_start = content.rfind('/**', 0, jsdoc_end)
    if jsdoc_start < 0:
        return None
    jsdoc = content[jsdoc_start:jsdoc_end + 2]

    param_types = {}
    param_descs = {}
    for m in re.finditer(r'@param\s*\{([^}]+)\}\s+(\w+)\s*-\s*(.*)', jsdoc):
        param_types[m.group(2)] = m.group(1)
        param_descs[m.group(2)] = m.group(3).strip()

    # Extract description
    desc_match = re.search(r'\*\s+([A-Z].*?)(?:\n|\*)', jsdoc)
    description = desc_match.group(1).strip().rstrip('.') if desc_match else func_name

    # Extract return type
    returns_match = re.search(r'@returns\s*\{([^}]+)\}\s*(.*)', jsdoc)
    return_type = returns_match.group(1) if returns_match else '*'
    return_desc = returns_match.group(2).strip() if returns_match else ''

    return {
        'name': func_name,
        'params': raw_params,
        'param_types': param_types,
        'param_descs': param_descs,
        'description': description,
        'return_type': return_type,
        'return_desc': return_desc,
    }


def ordinal(n):
    """Return ordinal string for an argument number (1-indexed)."""
    words = [None, 'First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth',
             'Seventh', 'Eighth', 'Ninth', 'Tenth', 'Eleventh', 'Twelfth',
             'Thirteenth', 'Fourteenth', 'Fifteenth', 'Sixteenth',
             'Seventeenth', 'Eighteenth', 'Nineteenth', 'Twentieth']
    if n < len(words):
        return words[n]
    return f'{n}th'


# ─── Code generation ─────────────────────────────────────────────────────

def generate_ndarray(info, pkg, routine):
    """Generate the ndarray.js content."""
    params = info['params']
    param_types = info['param_types']
    param_descs = info['param_descs']
    func_name = info['name']
    description = info['description']
    return_type = info['return_type']
    return_desc = info['return_desc']

    # Classify each parameter
    validations = []  # list of (param_name, arg_index, validation_type, details)
    requires = {}     # module_name -> require_path
    dim_params = []   # dimension parameters found

    for i, pname in enumerate(params):
        ptype = param_types.get(pname, '')
        arg_num = i + 1

        if ptype == 'string' or pname in STDLIB_VALIDATORS or pname in MANUAL_VALIDATORS:
            # Special case: uplo in routines that accept 'all'
            if pname == 'uplo' and func_name in UPLO_ACCEPTS_ALL:
                validations.append((pname, arg_num, 'manual', ['upper', 'lower', 'all'], 'a valid matrix triangle'))
            elif pname in STDLIB_VALIDATORS:
                validator_name, require_path, error_desc = STDLIB_VALIDATORS[pname]
                requires[validator_name] = require_path
                validations.append((pname, arg_num, 'stdlib', validator_name, error_desc))
            elif pname in MANUAL_VALIDATORS:
                valid_values, error_desc = MANUAL_VALIDATORS[pname]
                validations.append((pname, arg_num, 'manual', valid_values, error_desc))
        elif pname in DIMENSION_PARAMS and pname not in SKIP_DIMENSION_CHECK:
            dim_params.append((pname, arg_num))

    needs_format = bool(validations) or bool(dim_params)
    if needs_format:
        requires['format'] = '@stdlib/string/format'

    # Determine early return — use dimension params that exist
    early_return_dims = [p for p in ['M', 'N', 'K'] if p in params and p in DIMENSION_PARAMS]
    # Only use M and N for early return (K alone shouldn't trigger)
    early_return_dims = [p for p in ['M', 'N'] if p in params]
    # If only N exists (common in LAPACK), use just N
    if not early_return_dims:
        early_return_dims = [p for p in ['N'] if p in params]
    early_return_value = get_early_return_value(info, params)

    # Build requires section
    require_lines = []
    # Sort: stdlib validators first, then format, then base
    for name in sorted(requires.keys()):
        if name != 'format':
            require_lines.append(f"var {name} = require( '{requires[name]}' );")
    if 'format' in requires:
        require_lines.append(f"var format = require( '{requires['format']}' );")
    require_lines.append("var base = require( './base.js' );")

    # Build JSDoc
    jsdoc_lines = []
    jsdoc_lines.append('/**')
    jsdoc_lines.append(f'* {description}.')
    jsdoc_lines.append('*')

    for pname in params:
        ptype = param_types.get(pname, 'TODO')
        pdesc = param_descs.get(pname, 'TODO')
        jsdoc_lines.append(f'* @param {{{ptype}}} {pname} - {pdesc}')

    # Add @throws tags
    for pname, arg_num, vtype, *details in validations:
        ord_str = ordinal(arg_num).lower()
        if vtype == 'stdlib':
            error_desc = details[1]
            jsdoc_lines.append(f'* @throws {{TypeError}} {ord_str} argument must be {error_desc}')
        elif vtype == 'manual':
            error_desc = details[1]
            jsdoc_lines.append(f'* @throws {{TypeError}} {ord_str} argument must be {error_desc}')

    for pname, arg_num in dim_params:
        ord_str = ordinal(arg_num).lower()
        jsdoc_lines.append(f'* @throws {{RangeError}} {ord_str} argument must be a nonnegative integer')

    # Add @returns
    if return_desc:
        jsdoc_lines.append(f'* @returns {{{return_type}}} {return_desc}')
    else:
        jsdoc_lines.append(f'* @returns {{{return_type}}} result')

    jsdoc_lines.append('*/')

    # Build function body
    body_lines = []

    # Validation checks
    for pname, arg_num, vtype, *details in validations:
        ord_str = ordinal(arg_num)
        if vtype == 'stdlib':
            validator_name = details[0]
            error_desc = details[1]
            body_lines.append(f"\tif ( !{validator_name}( {pname} ) ) {{")
            body_lines.append(f"\t\tthrow new TypeError( format( 'invalid argument. {ord_str} argument must be {error_desc}. Value: `%s`.', {pname} ) );")
            body_lines.append('\t}')
        elif vtype == 'manual':
            valid_values = details[0]
            error_desc = details[1]
            conditions = ' && '.join(f"{pname} !== '{v}'" for v in valid_values)
            body_lines.append(f"\tif ( {conditions} ) {{")
            body_lines.append(f"\t\tthrow new TypeError( format( 'invalid argument. {ord_str} argument must be {error_desc}. Value: `%s`.', {pname} ) );")
            body_lines.append('\t}')

    for pname, arg_num in dim_params:
        ord_str = ordinal(arg_num)
        body_lines.append(f"\tif ( {pname} < 0 ) {{")
        body_lines.append(f"\t\tthrow new RangeError( format( 'invalid argument. {ord_str} argument must be a nonnegative integer. Value: `%d`.', {pname} ) );")
        body_lines.append('\t}')

    # Early return
    if early_return_dims and early_return_value is not None:
        cond = ' || '.join(f'{d} === 0' for d in early_return_dims)
        body_lines.append(f'\tif ( {cond} ) {{')
        body_lines.append(f'\t\treturn {early_return_value};')
        body_lines.append('\t}')
    elif early_return_dims and early_return_value is None:
        # Void return or skip early return
        cond = ' || '.join(f'{d} === 0' for d in early_return_dims)
        body_lines.append(f'\tif ( {cond} ) {{')
        body_lines.append(f'\t\treturn;')
        body_lines.append('\t}')

    # Call base
    call_args = ', '.join(params)
    body_lines.append(f'\treturn base( {call_args} );')

    # Determine if we need eslint-disable
    needs_eslint = len(params) > 5 or any(len(line) > 80 for line in body_lines)

    # Assemble file
    lines = []
    lines.append(LICENSE_HEADER)
    lines.append('')
    if needs_eslint:
        lines.append('/* eslint-disable max-len, max-params */')
        lines.append('')
    lines.append("'use strict';")
    lines.append('')
    lines.append('// MODULES //')
    lines.append('')
    for rl in require_lines:
        lines.append(rl)
    lines.append('')
    lines.append('')
    lines.append('// MAIN //')
    lines.append('')
    for jl in jsdoc_lines:
        lines.append(jl)
    sig = f'function {func_name}( {call_args} ) {{'
    lines.append(sig)
    for bl in body_lines:
        lines.append(bl)
    lines.append('}')
    lines.append('')
    lines.append('')
    lines.append('// EXPORTS //')
    lines.append('')
    lines.append(f'module.exports = {func_name};')
    lines.append('')

    return '\n'.join(lines)


def get_early_return_value(info, params):
    """Determine the early return value based on return type and params."""
    rt = info.get('return_type', '')
    rd = info.get('return_desc', '')

    # No @returns tag → void subroutine
    if not rt or rt == '*':
        return None  # signals: use bare `return;`

    # If return type mentions a specific array param, return it
    if 'Float64Array' in rt or 'Complex128Array' in rt:
        # Check return desc for backtick-quoted param name
        m = re.search(r'`(\w+)`', rd)
        if m and m.group(1) in params:
            return m.group(1)
        # Find the last array parameter (usually the output)
        for pname in reversed(params):
            ptype = info['param_types'].get(pname, '')
            if 'Float64Array' in ptype or 'Complex128Array' in ptype:
                if pname in ('C', 'B', 'y', 'Y'):
                    return pname

    if 'integer' in rt.lower() or 'Integer' in rt:
        return '0'
    if rt == 'number':
        return '0.0'
    if rt == 'boolean':
        return 'false'
    if rt == 'Object':
        return None  # skip early return for Object-returning routines
    return '0'


# ─── Main ────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description='Generate ndarray.js validation')
    parser.add_argument('--write', action='store_true', help='Apply changes')
    parser.add_argument('--preview', type=str, help='Show generated code for one routine')
    args = parser.parse_args()

    updated = 0
    skipped = 0
    errors = []

    for pkg in ['blas', 'lapack']:
        base_path = ROOT / 'lib' / pkg / 'base'
        if not base_path.is_dir():
            continue

        for routine in sorted(os.listdir(base_path)):
            module_dir = base_path / routine
            if not module_dir.is_dir():
                continue

            if args.preview and routine != args.preview:
                continue

            ndarray_path = module_dir / 'lib' / 'ndarray.js'
            base_js_path = module_dir / 'lib' / 'base.js'

            if not ndarray_path.is_file() or not base_js_path.is_file():
                skipped += 1
                continue

            # Skip if already has validation
            ndarray_content = ndarray_path.read_text()
            if 'throw' in ndarray_content and not args.preview:
                skipped += 1
                continue

            # Parse base.js
            try:
                info = parse_base_js(str(base_js_path))
                if not info:
                    errors.append(f'{pkg}/{routine}: could not parse base.js')
                    continue

                new_content = generate_ndarray(info, pkg, routine)

                if args.preview:
                    print(new_content)
                    return

                rel = str(ndarray_path.relative_to(ROOT))
                if args.write:
                    ndarray_path.write_text(new_content)
                    print(f'  Updated: {rel}')
                else:
                    print(f'  Would update: {rel}')
                updated += 1
            except Exception as e:
                errors.append(f'{pkg}/{routine}: {e}')

    if not args.preview:
        print(f'\n{"Updated" if args.write else "Would update"} {updated} files')
        print(f'Skipped {skipped} files (already validated or missing)')
        if errors:
            print(f'\nErrors ({len(errors)}):')
            for e in errors:
                print(f'  {e}')


if __name__ == '__main__':
    main()
