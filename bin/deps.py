#!/usr/bin/env python

"""
Analyze the dependency tree of a BLAS/LAPACK routine.

Usage:
  python bin/deps.py dgetrf
  python bin/deps.py dpotf2 --tree
  python bin/deps.py dgetrf --flat
  python bin/deps.py dgetrf --json

Scans the Fortran source for CALL statements and EXTERNAL declarations,
then recursively resolves dependencies from the BLAS/LAPACK source trees.
"""

import os
import re
import sys
import json
import argparse

# Routines to exclude from the dependency tree (intrinsics, utilities)
EXCLUDE = {
    'xerbla', 'xerbla_array',  # error handler (not a numerical routine)
    'lsame',                    # character comparison utility
}

# Intrinsic functions that look like EXTERNAL but aren't dependencies
INTRINSICS = {
    'max', 'min', 'abs', 'sqrt', 'mod', 'sign', 'dble', 'int', 'nint',
    'dabs', 'dmax1', 'dmin1', 'dsqrt', 'dSign',
    'conjg', 'dconjg', 'dimag',
    'ilaenv', 'ilaenv2stage',  # LAPACK query functions (config, not numerical)
}


def find_source(routine, data_dir):
    """Locate the Fortran source file for a routine."""
    candidates = [
        os.path.join(data_dir, 'BLAS-3.12.0', f'{routine}.f'),
        os.path.join(data_dir, 'lapack-3.12.0', 'SRC', f'{routine}.f'),
        os.path.join(data_dir, 'BLAS-3.12.0', f'{routine}.f90'),
        os.path.join(data_dir, 'lapack-3.12.0', 'SRC', f'{routine}.f90'),
        os.path.join(data_dir, 'lapack-3.12.0', 'INSTALL', f'{routine}.f'),
    ]
    for path in candidates:
        if os.path.exists(path):
            return path
    return None


def get_package(path, data_dir):
    """Determine if a source file is BLAS or LAPACK."""
    if 'BLAS' in path:
        return 'blas'
    return 'lapack'


def extract_deps(path):
    """Extract dependencies from a Fortran source file."""
    deps = set()
    with open(path) as f:
        content = f.read()

    # Remove comment lines (lines starting with * or C or !)
    lines = []
    for line in content.splitlines():
        stripped = line.lstrip()
        if stripped and stripped[0] in ('*', 'C', 'c', '!'):
            continue
        lines.append(line)
    clean = '\n'.join(lines)

    # Find CALL statements: CALL ROUTINE_NAME(
    for m in re.finditer(r'\bCALL\s+(\w+)', clean, re.IGNORECASE):
        name = m.group(1).lower()
        if name not in EXCLUDE and name not in INTRINSICS:
            deps.add(name)

    # Find EXTERNAL declarations for functions (not subroutines)
    for m in re.finditer(r'\bEXTERNAL\s+(?:::)?\s*(.+)', clean, re.IGNORECASE):
        names_str = m.group(1)
        for name in re.split(r'[,\s]+', names_str):
            name = name.strip().lower()
            if name and name not in EXCLUDE and name not in INTRINSICS:
                deps.add(name)

    return sorted(deps)


def build_tree(routine, data_dir, visited=None):
    """Recursively build the full dependency tree."""
    if visited is None:
        visited = set()

    routine = routine.lower()
    if routine in visited:
        return {'name': routine, 'circular': True}
    if routine in EXCLUDE or routine in INTRINSICS:
        return None

    visited.add(routine)

    path = find_source(routine, data_dir)
    if path is None:
        return {
            'name': routine,
            'package': 'unknown',
            'source': None,
            'deps': [],
        }

    package = get_package(path, data_dir)
    direct_deps = extract_deps(path)

    children = []
    for dep in direct_deps:
        child = build_tree(dep, data_dir, visited.copy())
        if child is not None:
            children.append(child)

    return {
        'name': routine,
        'package': package,
        'source': os.path.relpath(path, data_dir),
        'deps': children,
    }


def flatten_tree(tree, result=None, depth=0):
    """Flatten the tree into a unique sorted list with metadata."""
    if result is None:
        result = {}

    name = tree['name']
    if name not in result or depth < result[name]['depth']:
        result[name] = {
            'name': name,
            'package': tree.get('package', 'unknown'),
            'depth': depth,
        }

    for child in tree.get('deps', []):
        flatten_tree(child, result, depth + 1)

    return result


def to_graph(tree, nodes=None, edges=None):
    """Extract unique nodes and edges from the dependency tree."""
    if nodes is None:
        nodes = {}
    if edges is None:
        edges = set()

    name = tree['name']
    if name not in nodes:
        nodes[name] = {
            'id': name,
            'package': tree.get('package', 'unknown'),
        }
        if tree.get('source'):
            nodes[name]['source'] = tree['source']

    for child in tree.get('deps', []):
        edge = (name, child['name'])
        if edge not in edges:
            edges.add(edge)
        if not child.get('circular'):
            to_graph(child, nodes, edges)

    return nodes, edges


def print_tree(tree, indent=0, printed=None):
    """Pretty-print the dependency tree."""
    if printed is None:
        printed = set()

    name = tree['name']
    pkg = tree.get('package', '?')
    circular = tree.get('circular', False)

    prefix = '  ' * indent
    if circular:
        print(f'{prefix}{name} (circular)')
        return

    already = name in printed
    marker = ' (...)' if already else ''
    pkg_label = f'[{pkg}]' if pkg != 'unknown' else '[NOT FOUND]'

    print(f'{prefix}{name} {pkg_label}{marker}')

    if not already:
        printed.add(name)
        for child in tree.get('deps', []):
            print_tree(child, indent + 1, printed)


def main():
    parser = argparse.ArgumentParser(description='Analyze BLAS/LAPACK dependency tree')
    parser.add_argument('routine', help='Routine name (e.g., dgetrf, dpotf2)')
    parser.add_argument('--tree', action='store_true', default=True, help='Print as tree (default)')
    parser.add_argument('--flat', action='store_true', help='Print flat sorted list')
    parser.add_argument('--json', action='store_true', help='Output as JSON')
    parser.add_argument('--order', action='store_true', help='Print in dependency order (leaves first)')
    args = parser.parse_args()

    root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    data_dir = os.path.join(root_dir, 'data')

    tree = build_tree(args.routine.lower(), data_dir)

    if args.json:
        nodes, edges = to_graph(tree)
        print(json.dumps({
            'nodes': sorted(nodes.values(), key=lambda n: n['id']),
            'edges': [{'from': a, 'to': b} for a, b in sorted(edges)],
        }, indent=2))
    elif args.flat or args.order:
        flat = flatten_tree(tree)
        entries = sorted(flat.values(), key=lambda e: (-e['depth'], e['name']))
        if args.order:
            # Dependency order: deepest first (leaves before roots)
            for e in entries:
                pkg_label = f'[{e["package"]}]'
                print(f'  {e["name"]:<15} {pkg_label}')
        else:
            for e in entries:
                pkg_label = f'[{e["package"]}]'
                print(f'  {e["name"]:<15} {pkg_label:<10} depth={e["depth"]}')
    else:
        print_tree(tree)


if __name__ == '__main__':
    main()
