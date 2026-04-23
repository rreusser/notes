#!/usr/bin/env python
"""
Generate <routine>.js BLAS/LAPACK-style API wrappers from base.js signatures.

For each module with a stub wrapper, generates a real implementation that:
- Accepts order/layout param (for BLAS) or assumes column-major (for LAPACK internal)
- Converts LDA + order to strides
- Computes offsets from strides for vectors
- Calls base.js

Usage:
  python bin/gen_wrapper.py                    # fix all stubs
  python bin/gen_wrapper.py --dry-run          # preview
  python bin/gen_wrapper.py lib/blas/base/dgemm  # fix one module
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

LICENSE = ""


def parse_base_signature(base_path):
    """Extract function name and params from base.js."""
    with open(base_path) as f:
        content = f.read()

    # Find the function with @param (skip helpers)
    blocks = re.findall(r'/\*\*\n(.*?)\*/\s*\nfunction (\w+)\(\s*([^)]+)\s*\)', content, re.DOTALL)
    for block_body, func_name, params_str in blocks:
        if '@param' in block_body:
            params_str = re.sub(r'\s*//.*', '', params_str)
            params = [p.strip() for p in params_str.split(',') if p.strip()]
            # Parse @param types
            param_types = {}
            for line in block_body.split('\n'):
                m = re.match(r'\s*\* @param \{([^}]+)\}\s+(\w+)', line)
                if m:
                    param_types[m.group(2)] = m.group(1)
            return func_name, params, param_types

    # Fallback: just get function signature
    m = re.search(r'function (\w+)\(\s*([^)]+)\s*\)', content)
    if m:
        params_str = re.sub(r'\s*//.*', '', m.group(2))
        return m.group(1), [p.strip() for p in params_str.split(',')], {}

    return None, [], {}


def classify_params(params, param_types):
    """Classify base.js params into wrapper param groups."""
    groups = []  # list of (wrapper_params, base_call_args, setup_code)

    i = 0
    while i < len(params):
        p = params[i]
        ptype = param_types.get(p, '')

        # String params (uplo, trans, diag, side, etc.) - pass through
        if ptype == 'string' or p in ('uplo', 'trans', 'transa', 'transb', 'side', 'diag',
                                       'norm', 'job', 'compq', 'compz', 'jobvl', 'jobvr',
                                       'jobvs', 'jobu', 'jobvt', 'fact', 'equed', 'direct',
                                       'storev', 'vect', 'way', 'normin', 'howmny', 'sort',
                                       'range', 'jobz', 'itype', 'select'):
            groups.append(('passthrough', p, ptype))
            i += 1
            continue

        # Integer/scalar params - pass through
        if p in ('M', 'N', 'K', 'KL', 'KU', 'nrhs', 'ilo', 'ihi', 'nb', 'mm',
                 'alpha', 'beta', 'vl', 'vu', 'il', 'iu', 'abstol', 'rcond',
                 'anorm', 'k1', 'k2', 'inck', 'incx', 'kacc22', 'nshfts',
                 'iloz', 'ihiz', 'nh', 'nv', 'nw', 'ktop', 'kbot',
                 'offset', 'wantt', 'wantz', 'ca', 'd1', 'd2', 'wr', 'wi',
                 'smin', 'ltrans', 'na', 'nw', 'scond', 'amax', 'rowcnd', 'colcnd',
                 'ncc', 'ncvt', 'nru', 'kd', 'lwork', 'lrwork', 'liwork'):
            groups.append(('passthrough', p, ptype))
            i += 1
            continue

        # 2D matrix: A, strideA1, strideA2, offsetA -> A, LDA
        if (i + 3 < len(params) and
            re.match(r'stride' + re.escape(p) + r'1$', params[i+1]) and
            re.match(r'stride' + re.escape(p) + r'2$', params[i+2]) and
            re.match(r'offset' + re.escape(p) + r'$', params[i+3])):
            groups.append(('matrix2d', p, params[i+1], params[i+2], params[i+3]))
            i += 4
            continue

        # 1D vector: x, strideX, offsetX -> x, strideX (offset computed)
        if (i + 2 < len(params) and
            re.match(r'stride' + re.escape(p) + r'$', params[i+1], re.IGNORECASE) and
            re.match(r'offset' + re.escape(p) + r'$', params[i+2], re.IGNORECASE)):
            groups.append(('vector', p, params[i+1], params[i+2]))
            i += 3
            continue

        # Packed array: AP, strideAP, offsetAP -> AP (offset computed)
        if p == 'AP' and i + 2 < len(params) and 'strideAP' in params[i+1]:
            groups.append(('vector', p, params[i+1], params[i+2]))
            i += 3
            continue

        # Output scalars (Float64Array(1), Int32Array(1)) - pass through
        if 'Float64Array' in ptype or 'Int32Array' in ptype:
            if i + 2 < len(params) and 'stride' in params[i+1]:
                groups.append(('vector', p, params[i+1], params[i+2]))
                i += 3
            elif i + 1 < len(params) and 'offset' in params[i+1]:
                groups.append(('scalar_array', p, params[i+1]))
                i += 2
            else:
                groups.append(('passthrough', p, ptype))
                i += 1
            continue

        # Anything else - pass through
        groups.append(('passthrough', p, ptype))
        i += 1

    return groups


def generate_wrapper(routine, pkg, base_path):
    """Generate the <routine>.js wrapper content."""
    func_name, params, param_types = parse_base_signature(base_path)
    if not func_name:
        return None

    groups = classify_params(params, param_types)

    # Build wrapper params, setup code, and base call args
    wrapper_params = []
    setup_lines = []
    base_args = []
    var_decls = set()
    has_matrix = False
    has_vector = False
    needs_order = False  # BLAS routines need order param

    if pkg == 'blas':
        needs_order = any(g[0] == 'matrix2d' for g in groups)

    if needs_order:
        wrapper_params.append('order')

    for g in groups:
        if g[0] == 'passthrough':
            wrapper_params.append(g[1])
            base_args.append(g[1])
        elif g[0] == 'matrix2d':
            _, name, s1, s2, off = g
            has_matrix = True
            ld_name = 'LD' + name
            wrapper_params.append(name)
            wrapper_params.append(ld_name)
            var_s1 = 's' + name.lower() + '1'
            var_s2 = 's' + name.lower() + '2'
            var_decls.add(var_s1)
            var_decls.add(var_s2)
            base_args.extend([name, var_s1, var_s2, '0'])
        elif g[0] == 'vector':
            _, name, stride_name, offset_name = g
            has_vector = True
            wrapper_params.append(name)
            wrapper_params.append(stride_name)
            var_off = 'o' + name.lower()
            var_decls.add(var_off)
            base_args.extend([name, stride_name, var_off])
        elif g[0] == 'scalar_array':
            _, name, offset_name = g
            wrapper_params.append(name)
            base_args.extend([name, '0'])

    # Generate stride setup
    if has_matrix:
        matrix_groups = [g for g in groups if g[0] == 'matrix2d']
        for g in matrix_groups:
            _, name, s1, s2, off = g
            ld_name = 'LD' + name
            vs1 = 's' + name.lower() + '1'
            vs2 = 's' + name.lower() + '2'
            if needs_order:
                setup_lines.append(f"\tif ( order === 'column-major' ) {{")
                setup_lines.append(f'\t\t{vs1} = 1;')
                setup_lines.append(f'\t\t{vs2} = {ld_name};')
                setup_lines.append('\t} else {')
                setup_lines.append(f'\t\t{vs1} = {ld_name};')
                setup_lines.append(f'\t\t{vs2} = 1;')
                setup_lines.append('\t}')
            else:
                # LAPACK: always column-major
                setup_lines.append(f'\t{vs1} = 1;')
                setup_lines.append(f'\t{vs2} = {ld_name};')

    # Generate offset computation for vectors
    if has_vector:
        vector_groups = [g for g in groups if g[0] == 'vector']
        for g in vector_groups:
            _, name, stride_name, offset_name = g
            var_off = 'o' + name.lower()
            # Find dimension param - use N as default
            dim = 'N'
            setup_lines.append(f'\t{var_off} = stride2offset( {dim}, {stride_name} );')

    # Build var declarations
    sorted_vars = sorted(var_decls, key=lambda v: (-len(v), v))

    # Build JSDoc
    jsdoc_lines = ['/**']
    # Get description from base.js
    with open(base_path) as f:
        base_content = f.read()
    desc_match = re.search(r'/\*\*\n\s*\*(.*?)\n\s*\*\s*\n', base_content, re.DOTALL)
    if desc_match:
        desc = desc_match.group(1).strip().lstrip('* ').split('\n')[0]
        jsdoc_lines.append(f'* {desc}')
    else:
        jsdoc_lines.append(f'* BLAS/LAPACK-style API for {routine}.')
    jsdoc_lines.append('*')

    for wp in wrapper_params:
        if wp == 'order':
            jsdoc_lines.append("* @param {string} order - storage layout (`'row-major'` or `'column-major'`)")
        elif wp.startswith('LD'):
            jsdoc_lines.append(f'* @param {{PositiveInteger}} {wp} - leading dimension of `{wp[2:]}`')
        elif wp in param_types:
            pt = param_types[wp]
            jsdoc_lines.append(f'* @param {{{pt}}} {wp} - {wp}')
        else:
            jsdoc_lines.append(f'* @param {{*}} {wp} - {wp}')

    if needs_order:
        jsdoc_lines.append('* @throws {TypeError} first argument must be a valid order')
    jsdoc_lines.append('* @returns {*} result')
    jsdoc_lines.append('*/')

    # Assemble the file
    lines = [LICENSE, '', "'use strict';", '', '// MODULES //', '']

    requires = ["var base = require( './base.js' );"]
    if needs_order:
        requires.insert(0, "var isLayout = require( '@stdlib/blas/base/assert/is-layout' );")
    if has_vector:
        requires.insert(-1 if not needs_order else 1, "var stride2offset = require( '@stdlib/strided/base/stride2offset' );")
    if needs_order:
        requires.insert(len(requires) - 1, "var format = require( '@stdlib/string/format' );")

    lines.extend(requires)
    lines.extend(['', '', '// MAIN //', ''])
    lines.extend(jsdoc_lines)

    eslint = ''
    if len(wrapper_params) > 6:
        eslint = ' // eslint-disable-line max-len, max-params'

    lines.append(f'function {routine}( {", ".join(wrapper_params)} ) {{{eslint}')

    if sorted_vars:
        for v in sorted_vars:
            lines.append(f'\tvar {v};')

    if needs_order:
        lines.append('')
        lines.append("\tif ( !isLayout( order ) ) {")
        lines.append("\t\tthrow new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );")
        lines.append('\t}')

    if setup_lines:
        lines.append('')
        lines.extend(setup_lines)

    base_call = f'\treturn base( {", ".join(base_args)} );'
    if len(base_call) > 100:
        base_call += ' // eslint-disable-line max-len'
    lines.append(base_call)
    lines.append('}')
    lines.extend(['', '', '// EXPORTS //', '', f'module.exports = {routine};', ''])

    return '\n'.join(lines)


def main():
    dry_run = '--dry-run' in sys.argv
    target = None
    for arg in sys.argv[1:]:
        if not arg.startswith('-') and os.path.isdir(arg):
            target = arg

    count = 0
    for pkg in ['blas', 'lapack']:
        base_dir = os.path.join(ROOT, 'lib', pkg, 'base')
        if not os.path.isdir(base_dir):
            continue
        for routine in sorted(os.listdir(base_dir)):
            routine_dir = os.path.join(base_dir, routine)
            if target and routine_dir != os.path.abspath(target):
                continue

            wrapper_path = os.path.join(routine_dir, 'lib', f'{routine}.js')
            base_path = os.path.join(routine_dir, 'lib', 'base.js')

            if not os.path.exists(wrapper_path) or not os.path.exists(base_path):
                continue

            with open(wrapper_path) as f:
                content = f.read()
            if 'not yet implemented' not in content:
                continue

            result = generate_wrapper(routine, pkg, base_path)
            if result:
                if dry_run:
                    print(f'  Would fix: {wrapper_path}')
                else:
                    with open(wrapper_path, 'w') as f:
                        f.write(result)
                    print(f'  Fixed: {routine}')
                count += 1

    action = 'Would fix' if dry_run else 'Fixed'
    print(f'\n{action} {count} wrapper files')


if __name__ == '__main__':
    main()
