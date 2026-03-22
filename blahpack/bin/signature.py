#!/usr/bin/env python

"""
Parse a Fortran BLAS/LAPACK source file and emit the expected stdlib-js
call signature for its base.js implementation.

Usage:
  python signature.py BLAS-3.12.0/daxpy.f
  python signature.py lapack-3.12.0/SRC/dpotf2.f
  python signature.py BLAS-3.12.0/d*.f          # batch mode

Output (JSON):
  {
    "name": "daxpy",
    "kind": "subroutine",
    "fortran_args": ["N", "DA", "DX", "INCX", "DY", "INCY"],
    "js_args": ["N", "alpha", "x", "strideX", "offsetX", "y", "strideY", "offsetY"],
    "returns": "void",
    "arrays": { "DX": {"dim": 1, "shape": ["*"], "inc": "INCX"}, ... },
    "consumed": ["INCX", "INCY"]
  }
"""

import os
import re
import sys
import json
import glob
import argparse
from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk, get_child
from fparser.two.Fortran2003 import (
    Assumed_Size_Spec,
    Dummy_Arg_List,
    Entity_Decl,
    Entity_Decl_List,
    Explicit_Shape_Spec,
    Explicit_Shape_Spec_List,
    Function_Stmt,
    Function_Subprogram,
    Int_Literal_Constant,
    Intrinsic_Type_Spec,
    Name,
    Specification_Part,
    Subroutine_Stmt,
    Subroutine_Subprogram,
    Type_Declaration_Stmt,
)


# Conventional scalar renames (Fortran arg name → JS name)
SCALAR_RENAMES = {
    'da': 'alpha',
    'db': 'beta',
}

# BLAS vector naming: strip precision prefix (D/S/C/Z) from vector names
# DX → x, DY → y, DL → DL (only strip single-char suffixes)
def strip_blas_prefix(name):
    """Strip BLAS precision prefix from vector names: DX→X, DY→Y.
    Only strips when result is a single letter (X, Y, A, B).
    Preserves multi-char names: DL→DL, DU→DU, DU2→DU2, IPIV→IPIV."""
    if len(name) == 2 and name[0] in 'DSCZ' and name[1] in 'XYZAB':
        return name[1]
    return name


def parse_file(path):
    with open(path) as f:
        code = f.read()

    reader = FortranStringReader(code, ignore_comments=False)
    parser = ParserFactory().create(std="f2003")
    ast = parser(reader)
    return ast


def find_routine(ast):
    """Find the first subroutine or function in the AST."""
    for node in walk(ast):
        if isinstance(node, (Subroutine_Subprogram, Function_Subprogram)):
            return node
    return None


def get_routine_info(routine_node):
    """Extract name, kind, and dummy args from a routine node."""
    if isinstance(routine_node, Subroutine_Subprogram):
        kind = 'subroutine'
        stmt_type = Subroutine_Stmt
    else:
        kind = 'function'
        stmt_type = Function_Stmt

    stmt = None
    for child in routine_node.children:
        if isinstance(child, stmt_type):
            stmt = child
            break

    name = stmt.get_name().string.lower()

    dummy_list = get_child(stmt, Dummy_Arg_List)
    if dummy_list is not None:
        args = [arg.string.upper() for arg in dummy_list.items]
    else:
        args = []

    return name, kind, args


def get_declarations(routine_node):
    """Parse all type declarations from the specification part."""
    spec = None
    for child in routine_node.children:
        if isinstance(child, Specification_Part):
            spec = child
            break

    if spec is None:
        return {}

    decls = {}
    for child in spec.children:
        if isinstance(child, Type_Declaration_Stmt):
            # Get the type
            type_spec = None
            for c in walk(child):
                if isinstance(c, Intrinsic_Type_Spec):
                    type_spec = c
                    break

            if type_spec is None:
                continue

            type_str = type_spec.string.lower()

            # Get entity declarations
            for c in walk(child):
                if isinstance(c, Entity_Decl):
                    decl_name = c.get_name().string.upper()

                    # Check for array shape
                    shape = None
                    aspec = get_child(c, Assumed_Size_Spec)

                    if aspec is not None:
                        shape = []
                        for child0 in aspec.children:
                            if child0 is None:
                                pass
                            elif isinstance(child0, Explicit_Shape_Spec_List):
                                for child1 in child0.children:
                                    if isinstance(child1, Explicit_Shape_Spec):
                                        name_node = get_child(child1, Name)
                                        if name_node:
                                            shape.append(name_node.string.upper())
                                        else:
                                            int_node = get_child(child1, Int_Literal_Constant)
                                            if int_node:
                                                shape.append(int(int_node.string))
                        shape.append('*')
                    else:
                        espec = get_child(c, Explicit_Shape_Spec_List)
                        if espec is not None:
                            shape = []
                            for child0 in espec.children:
                                if isinstance(child0, Explicit_Shape_Spec):
                                    int_node = get_child(child0, Int_Literal_Constant)
                                    if int_node:
                                        shape.append(int(int_node.string))
                                    else:
                                        name_node = get_child(child0, Name)
                                        if name_node:
                                            shape.append(name_node.string.upper())

                    decls[decl_name] = {
                        'type': type_str,
                        'shape': shape,
                    }

    return decls


def classify_arg(arg_name, decls, all_args):
    """Classify a Fortran argument as scalar, 1d-array, 2d-array, or stride/info."""
    decl = decls.get(arg_name, {})
    shape = decl.get('shape')

    if shape is not None and len(shape) > 0 and shape[-1] == '*':
        dim = len(shape)
        leading_dims = [s for s in shape if s != '*']
        return {
            'role': f'{dim}d-array',
            'dim': dim,
            'shape': shape,
            'leading_dims': leading_dims,  # e.g., ['LDA'] for A(LDA,*)
        }

    if shape is not None and len(shape) > 0:
        return {
            'role': 'fixed-array',
            'dim': len(shape),
            'shape': shape,
        }

    return {'role': 'scalar'}


def is_inc_for_array(inc_name, arrays, all_args):
    """Check if a scalar arg like INCX is a stride for a known array."""
    # Pattern: INC{X} matches array {X} or D{X}
    m = re.match(r'^INC([A-Z]+)$', inc_name)
    if m:
        suffix = m.group(1)
        # Check if there's an array named D{suffix} or {suffix}
        for arr_name in arrays:
            arr_base = re.sub(r'^D', '', arr_name)
            if arr_base == suffix or arr_name == suffix:
                return arr_name
    return None


def generate_signature(path):
    """Generate the stdlib-js signature for a Fortran source file."""
    ast = parse_file(path)
    routine = find_routine(ast)
    if routine is None:
        return None

    name, kind, fortran_args = get_routine_info(routine)
    decls = get_declarations(routine)

    # Classify each argument
    classifications = {}
    for arg in fortran_args:
        classifications[arg] = classify_arg(arg, decls, fortran_args)

    # Identify arrays
    arrays = {}
    for arg in fortran_args:
        cl = classifications[arg]
        if cl['role'] in ('1d-array', '2d-array'):
            arrays[arg] = cl

    # Identify consumed args (LDA/LDB → absorbed by strides, INCX/INCY → become strides, INFO → return)
    consumed = set()
    inc_map = {}  # INC arg → array name

    for arg in fortran_args:
        cl = classifications[arg]

        # Leading dimensions (LDA, LDB, etc.) consumed by 2D arrays
        if cl['role'] == 'scalar':
            for arr_name, arr_info in arrays.items():
                if arg in arr_info.get('leading_dims', []):
                    consumed.add(arg)

        # INC args consumed by 1D arrays
        if cl['role'] == 'scalar':
            arr_name = is_inc_for_array(arg, arrays, fortran_args)
            if arr_name:
                consumed.add(arg)
                inc_map[arg] = arr_name

    # INFO is consumed (becomes return value) for LAPACK routines
    has_info = 'INFO' in fortran_args
    if has_info:
        consumed.add('INFO')

    # Count total arrays to decide naming (single-array routines use plain stride/offset)
    array_count = len(arrays)

    # Map Fortran types to JSDoc types
    FORTRAN_TO_JSDOC = {
        'integer': 'integer',
        'double precision': 'number',
        'real': 'number',
        'real(wp)': 'number',
        'logical': 'boolean',
        'character': 'string',
        'complex*16': 'Complex128',
    }

    # Map Fortran float array types to JS typed array names
    FORTRAN_TO_TYPED_ARRAY = {
        'double precision': 'Float64Array',
        'real': 'Float32Array',
        'real(wp)': 'Float64Array',
        'integer': 'Int32Array',
    }

    def add_arg(name, jsdoc_type, description):
        js_args.append(name)
        js_arg_meta.append({'name': name, 'type': jsdoc_type, 'desc': description})

    # Build JS argument list with metadata
    js_args = []
    js_arg_meta = []
    for arg in fortran_args:
        if arg in consumed:
            continue

        cl = classifications[arg]
        decl = decls.get(arg, {})
        fortran_type = decl.get('type', 'double precision')

        if cl['role'] in ('1d-array', '2d-array'):
            dim = cl['dim']
            js_base = strip_blas_prefix(arg)
            typed_array = FORTRAN_TO_TYPED_ARRAY.get(fortran_type, 'Float64Array')

            if dim == 1:
                if len(js_base) == 1:
                    arr_js = js_base.lower()
                else:
                    arr_js = js_base

                # Heuristic: the last array in the arg list is likely the output
                is_last_array = (arg == list(arrays.keys())[-1]) and len(arrays) > 1
                arr_role = 'output array' if is_last_array else 'input array'
                add_arg(arr_js, typed_array, arr_role)
                if array_count == 1:
                    add_arg('stride', 'integer', f'stride length for `{arr_js}`')
                    add_arg('offset', 'NonNegativeInteger', f'starting index for `{arr_js}`')
                else:
                    add_arg(f'stride{js_base}', 'integer', f'stride length for `{arr_js}`')
                    add_arg(f'offset{js_base}', 'NonNegativeInteger', f'starting index for `{arr_js}`')
            else:
                is_last_array = (arg == list(arrays.keys())[-1]) and len(arrays) > 1
                mat_role = 'output matrix' if is_last_array else 'input matrix'
                add_arg(js_base, typed_array, mat_role)
                add_arg(f'stride{js_base}1', 'integer', f'stride of the first dimension of `{js_base}`')
                add_arg(f'stride{js_base}2', 'integer', f'stride of the second dimension of `{js_base}`')
                add_arg(f'offset{js_base}', 'NonNegativeInteger', f'starting index for `{js_base}`')
        elif cl['role'] == 'fixed-array':
            js_base = strip_blas_prefix(arg)
            typed_array = FORTRAN_TO_TYPED_ARRAY.get(fortran_type, 'Float64Array')
            add_arg(js_base.lower(), typed_array, f'input array')
            add_arg(f'stride{js_base}', 'integer', f'stride length for `{js_base.lower()}`')
            add_arg(f'offset{js_base}', 'NonNegativeInteger', f'starting index for `{js_base.lower()}`')
        else:
            js_name = SCALAR_RENAMES.get(arg.lower(), arg.lower())
            jsdoc_type = FORTRAN_TO_JSDOC.get(fortran_type, 'number')

            # Refine type and description based on role
            if fortran_type == 'integer' and re.match(r'^[NMK]$', arg):
                js_name = arg
                jsdoc_type = 'NonNegativeInteger'
                desc_map = {'N': 'number of columns', 'M': 'number of rows', 'K': 'number of superdiagonals'}
                desc = desc_map.get(arg, f'dimension parameter')
            elif fortran_type == 'character':
                js_name = arg.lower()
                desc = f'specifies the operation type'
            elif js_name == 'alpha':
                desc = 'scalar constant'
            elif js_name == 'beta':
                desc = 'scalar constant'
            else:
                desc = arg.lower()

            add_arg(js_name, jsdoc_type, desc)

    # Determine return type
    if kind == 'function':
        returns = decls.get(name.upper(), {}).get('type', 'number')
    elif has_info:
        returns = 'integer'
    else:
        returns = 'void'

    # Build array info for output
    array_info = {}
    for arr_name, arr_cl in arrays.items():
        info = {
            'dim': arr_cl['dim'],
            'shape': arr_cl['shape'],
        }
        # Find which INC arg maps to this array
        for inc_arg, target in inc_map.items():
            if target == arr_name:
                info['inc'] = inc_arg
        # Find leading dimension args
        if arr_cl.get('leading_dims'):
            info['leading_dims'] = arr_cl['leading_dims']
        array_info[arr_name] = info

    # Determine return type JSDoc
    if kind == 'function':
        ft = decls.get(name.upper(), {}).get('type', 'double precision')
        returns_jsdoc = FORTRAN_TO_JSDOC.get(ft, 'number')
    elif has_info:
        returns_jsdoc = 'integer'
    else:
        returns_jsdoc = None

    return {
        'name': name,
        'kind': kind,
        'fortran_sig': f'{kind.upper()} {name.upper()}({", ".join(fortran_args)})',
        'fortran_args': fortran_args,
        'js_args': js_args,
        'js_arg_meta': js_arg_meta,
        'js_sig': f'function {name}( {", ".join(js_args)} )',
        'returns': returns,
        'returns_jsdoc': returns_jsdoc,
        'arrays': array_info,
        'consumed': sorted(consumed),
    }


def main():
    parser = argparse.ArgumentParser(description='Generate stdlib-js call signatures from Fortran source')
    parser.add_argument('files', nargs='+', help='Fortran source files (supports globs)')
    parser.add_argument('--json', action='store_true', help='Output full JSON (default: summary)')
    parser.add_argument('--compact', action='store_true', help='One-line output per routine')
    args = parser.parse_args()

    # Expand globs
    paths = []
    for pattern in args.files:
        expanded = glob.glob(pattern)
        if expanded:
            paths.extend(sorted(expanded))
        else:
            paths.append(pattern)

    results = []
    for path in paths:
        if not os.path.isfile(path):
            print(f'Warning: {path} not found', file=sys.stderr)
            continue

        try:
            sig = generate_signature(path)
        except Exception as e:
            print(f'Error parsing {path}: {e}', file=sys.stderr)
            continue

        if sig is None:
            continue

        results.append(sig)

        if args.json:
            continue

        if args.compact:
            consumed_str = f'  (consumed: {", ".join(sig["consumed"])})' if sig['consumed'] else ''
            ret_str = f' → {sig["returns"]}' if sig['returns'] != 'void' else ''
            print(f'{sig["js_sig"]}{ret_str}{consumed_str}')
        else:
            print(f'  Fortran: {sig["fortran_sig"]}')
            print(f'  JS:      {sig["js_sig"]}')
            if sig['returns'] != 'void':
                print(f'  Returns: {sig["returns"]}')
            if sig['consumed']:
                print(f'  Consumed: {", ".join(sig["consumed"])}')
            if sig['arrays']:
                for arr_name, arr_info in sig['arrays'].items():
                    inc_str = f', inc={arr_info["inc"]}' if 'inc' in arr_info else ''
                    ld_str = f', leading_dims={arr_info["leading_dims"]}' if 'leading_dims' in arr_info else ''
                    print(f'  Array {arr_name}: {arr_info["dim"]}D, shape={arr_info["shape"]}{inc_str}{ld_str}')
            print()

    if args.json:
        print(json.dumps(results, indent=2))


if __name__ == '__main__':
    main()
