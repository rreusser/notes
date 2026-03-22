#!/usr/bin/env python

"""
Composable code-mod pipeline for Fortran→JS translation.

Usage:
  python transform.py <pipeline-dir> --init <package> <routine>
  python transform.py <pipeline-dir> --apply <transform> [--apply <transform> ...]
  python transform.py <pipeline-dir> --config <path>
  python transform.py <pipeline-dir> --verify <step>
  python transform.py --list

Each transform reads the highest-numbered file in the pipeline directory
and writes the next numbered file. A transforms.json manifest tracks
what was applied at each step.

Example:
  python transform.py pipeline/dpotf2 --init lapack dpotf2
  python transform.py pipeline/dpotf2 --apply remove-trivial-goto --apply free-form
  python transform.py pipeline/dpotf2 --apply translate-to-js
  python transform.py pipeline/dpotf2 --verify 1
"""

import os
import re
import sys
import json
import glob
import shutil
import argparse
import subprocess

# ---------------------------------------------------------------------------
# Transform registry
# ---------------------------------------------------------------------------

TRANSFORMS = {}

def register(name, input_type, output_type, description):
    """Decorator to register a transform function."""
    def decorator(fn):
        TRANSFORMS[name] = {
            'fn': fn,
            'input_type': input_type,
            'output_type': output_type,
            'description': description,
        }
        return fn
    return decorator

def extension_for(file_type):
    return '.f90' if file_type == 'fortran' else '.js'

# ---------------------------------------------------------------------------
# Fortran parsing helpers (shared by Fortran transforms)
# ---------------------------------------------------------------------------

def parse_fortran(code):
    """Parse Fortran source into an fparser AST."""
    from fparser.common.readfortran import FortranStringReader
    from fparser.two.parser import ParserFactory
    reader = FortranStringReader(code, ignore_comments=False)
    parser = ParserFactory().create(std="f2003")
    return parser(reader)

def analyze_gotos(ast):
    """Analyze an fparser AST for GOTO-related constructs. Returns dicts."""
    from fparser.two.utils import walk
    from fparser.two.Fortran2003 import (
        Goto_Stmt, Computed_Goto_Stmt, Label, Label_List,
        Label_Do_Stmt, Continue_Stmt, If_Stmt,
    )

    continue_stmts = {}
    goto_stmts = {}
    do_stmts = {}
    if_stmts = {}

    for node in walk(ast):
        if isinstance(node, Continue_Stmt):
            label = str(node.item.label)
            continue_stmts[label] = node

        if isinstance(node, Goto_Stmt):
            from fparser.two.Fortran2003 import Label as LabelCls
            children = [c for c in node.children if isinstance(c, LabelCls)]
            if children:
                label = children[0].string
                goto_stmts.setdefault(label, []).append(node)

        if isinstance(node, Computed_Goto_Stmt):
            labels = [c for c in node.children if isinstance(c, Label_List)]
            if labels:
                for label_node in labels[0].items:
                    label = label_node.string
                    goto_stmts.setdefault(label, []).append(node)

        if isinstance(node, Label_Do_Stmt):
            children = [c for c in node.children if isinstance(c, Label)]
            if children:
                label = children[0].string
                do_stmts[label] = node

        if isinstance(node, If_Stmt):
            potential_goto = node.items[1]
            if isinstance(potential_goto, Goto_Stmt):
                children = [c for c in potential_goto.children if isinstance(c, Label)]
                if children:
                    label = children[0].string
                    if_stmts.setdefault(label, node)

    return continue_stmts, goto_stmts, do_stmts, if_stmts

# ---------------------------------------------------------------------------
# Transform: remove-trivial-goto
# ---------------------------------------------------------------------------

@register('remove-trivial-goto', 'fortran', 'fortran',
          'Convert label-DO/CONTINUE loops to nonlabel-DO/END-DO')
def remove_trivial_goto(source, **kwargs):
    from fparser.two.Fortran2003 import (
        Nonlabel_Do_Stmt, Loop_Control, End_Do_Stmt, Continue_Stmt,
    )

    ast = parse_fortran(source)
    continue_stmts, goto_stmts, do_stmts, if_stmts = analyze_gotos(ast)

    count = 0
    to_delete = []
    for label, labeldo_node in do_stmts.items():
        continue_cnt = 1 if label in continue_stmts else 0
        goto_cnt = len(goto_stmts[label]) if label in goto_stmts else 0

        if continue_cnt != 1 or goto_cnt != 0:
            continue

        continue_node = continue_stmts[label]
        assert isinstance(continue_node, Continue_Stmt)

        loopctrl_node = labeldo_node.items[2]
        assert isinstance(loopctrl_node, Loop_Control)

        nonlabel_do = Nonlabel_Do_Stmt('DO')
        items = list(nonlabel_do.items)
        items[1] = loopctrl_node
        nonlabel_do.items = tuple(items)

        labeldo_idx = labeldo_node.parent.children.index(labeldo_node)
        labeldo_node.parent.children[labeldo_idx] = nonlabel_do

        end_do_node = End_Do_Stmt('END DO')
        continue_idx = continue_node.parent.children.index(continue_node)
        continue_node.parent.children[continue_idx] = end_do_node

        to_delete.append(label)
        count += 1

    eprint(f'  remove-trivial-goto: replaced {count} label-DO loops')
    return str(ast)

# ---------------------------------------------------------------------------
# Transform: remove-trivial-if-goto
# ---------------------------------------------------------------------------

@register('remove-trivial-if-goto', 'fortran', 'fortran',
          'Remove trivial IF(cond) GO TO statements with unreferenced labels')
def remove_trivial_if_goto(source, **kwargs):
    from fparser.two.Fortran2003 import Goto_Stmt, Label

    ast = parse_fortran(source)
    continue_stmts, goto_stmts, do_stmts, if_stmts = analyze_gotos(ast)

    count = 0
    to_delete = []
    for label, if_node in if_stmts.items():
        goto_node = if_node.items[1]
        assert isinstance(goto_node, Goto_Stmt)
        children = [c for c in goto_node.children if isinstance(c, Label)]
        label = children[0].string

        if label in goto_stmts:
            continue
        to_delete.append(label)
        count += 1

    for label in to_delete:
        del if_stmts[label]

    eprint(f'  remove-trivial-if-goto: removed {count} trivial IF GOTOs')
    return str(ast)

# ---------------------------------------------------------------------------
# Transform: free-form
# ---------------------------------------------------------------------------

@register('free-form', 'fortran', 'fortran',
          'Convert fixed-form Fortran to free-form (comments, indentation)')
def free_form(source, **kwargs):
    lines = source.splitlines()
    formatted = []
    for line in lines:
        stripped = line.strip()
        if stripped.startswith('*'):
            formatted.append('!' + stripped[1:])
        elif stripped == '':
            formatted.append('')
        else:
            formatted.append('      ' + stripped)
    return '\n'.join(formatted)

# ---------------------------------------------------------------------------
# Transform: translate-to-js
# ---------------------------------------------------------------------------

@register('translate-to-js', 'fortran', 'js',
          'Translate Fortran to JavaScript via ESTree (1-based output)')
def translate_to_js(source, **kwargs):
    root_dir = kwargs.get('root_dir', os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    bin_dir = os.path.dirname(os.path.abspath(__file__))
    estree_script = os.path.join(bin_dir, 'fortran_to_estree.py')
    js_script = os.path.join(bin_dir, 'estree_to_js.js')

    # Pipe: source → fortran_to_estree.py → estree_to_js.js
    p1 = subprocess.Popen(
        [sys.executable, estree_script],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL
    )
    p2 = subprocess.Popen(
        ['node', js_script],
        stdin=p1.stdout, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    p1.stdout.close()
    p1.stdin.write(source.encode())
    p1.stdin.close()
    js_output, js_err = p2.communicate()

    if p2.returncode != 0:
        eprint(f'  translate-to-js: error: {js_err.decode()}')
        sys.exit(1)

    eprint(f'  translate-to-js: complete')
    return js_output.decode()

# ---------------------------------------------------------------------------
# Pipeline helpers
# ---------------------------------------------------------------------------

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def find_latest_step(pipeline_dir, routine):
    """Find the highest-numbered file in the pipeline directory."""
    pattern = os.path.join(pipeline_dir, f'{routine}.[0-9][0-9].*')
    files = sorted(glob.glob(pattern))
    if not files:
        return None, None, None
    latest = files[-1]
    basename = os.path.basename(latest)
    # Parse: routine.NN.ext
    match = re.match(rf'^{re.escape(routine)}\.(\d+)\.(.*)', basename)
    if not match:
        return None, None, None
    step = int(match.group(1))
    ext = match.group(2)
    file_type = 'js' if ext == 'js' else 'fortran'
    return step, latest, file_type

def load_manifest(pipeline_dir):
    path = os.path.join(pipeline_dir, 'transforms.json')
    if os.path.exists(path):
        with open(path) as f:
            return json.load(f)
    return {'steps': []}

def save_manifest(pipeline_dir, manifest):
    path = os.path.join(pipeline_dir, 'transforms.json')
    with open(path, 'w') as f:
        json.dump(manifest, f, indent=2)
        f.write('\n')

def get_routine_from_dir(pipeline_dir):
    """Infer routine name from the transforms.json manifest or directory name."""
    manifest = load_manifest(pipeline_dir)
    if manifest.get('steps'):
        # Extract routine from first file name: "dpotf2.00.f" → "dpotf2"
        first_file = manifest['steps'][0].get('file', '')
        match = re.match(r'^([^.]+)\.', first_file)
        if match:
            return match.group(1)
    return os.path.basename(os.path.normpath(pipeline_dir))

def get_package(pipeline_dir):
    """Read package from manifest."""
    manifest = load_manifest(pipeline_dir)
    return manifest.get('package')

# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------

def cmd_init(pipeline_dir, package, routine):
    """Initialize a pipeline: copy source to routine.00.f"""
    root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    os.makedirs(pipeline_dir, exist_ok=True)

    if package == 'blas':
        source = os.path.join(root_dir, 'data', 'BLAS-3.12.0', f'{routine}.f')
    elif package == 'lapack':
        source = os.path.join(root_dir, 'data', 'lapack-3.12.0', 'SRC', f'{routine}.f')
    else:
        eprint(f"Unknown package: {package}")
        sys.exit(1)

    if not os.path.exists(source):
        eprint(f"Source not found: {source}")
        sys.exit(1)

    dest = os.path.join(pipeline_dir, f'{routine}.00.f')
    shutil.copy2(source, dest)
    eprint(f'Initialized: {dest}')

    manifest = {
        'package': package,
        'source': source,
        'steps': [
            {'step': 0, 'file': f'{routine}.00.f', 'transform': 'original'}
        ]
    }
    save_manifest(pipeline_dir, manifest)

    # Generate reference fixture if test exists
    test_src = os.path.join(root_dir, 'test', 'fortran', f'test_{routine}.f90')
    if os.path.exists(test_src):
        harness = os.path.join(root_dir, 'test', 'run_fortran.sh')
        subprocess.run([harness, package, routine], check=True)
        eprint('Reference fixture generated.')
    else:
        eprint(f'No test program found (test/fortran/test_{routine}.f90). Skipping fixture.')

def cmd_apply(pipeline_dir, transform_names, from_step=None):
    """Apply one or more transforms in sequence."""
    routine = get_routine_from_dir(pipeline_dir)
    manifest = load_manifest(pipeline_dir)
    root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    for transform_name in transform_names:
        if transform_name not in TRANSFORMS:
            eprint(f"Unknown transform: {transform_name}")
            eprint(f"Available: {', '.join(sorted(TRANSFORMS.keys()))}")
            sys.exit(1)

        transform = TRANSFORMS[transform_name]

        # Find latest step
        step, latest_path, file_type = find_latest_step(pipeline_dir, routine)
        if step is None:
            eprint(f"No files found in {pipeline_dir}. Run --init first.")
            sys.exit(1)

        if from_step is not None:
            # Override: start from a specific step
            ext = 'f90' if transform['input_type'] == 'fortran' else 'js'
            candidates = glob.glob(os.path.join(pipeline_dir, f'{routine}.{from_step:02d}.*'))
            if not candidates:
                # Try .f extension too
                candidates = glob.glob(os.path.join(pipeline_dir, f'{routine}.{from_step:02d}.*'))
            if not candidates:
                eprint(f"No file found for step {from_step}")
                sys.exit(1)
            latest_path = candidates[0]
            step = from_step
            from_step = None  # Only use --from for the first transform

        # Check type compatibility
        if transform['input_type'] != file_type:
            eprint(f"Type mismatch: {transform_name} expects {transform['input_type']}, "
                   f"but step {step} is {file_type}")
            sys.exit(1)

        # Read input
        with open(latest_path) as f:
            source = f.read()

        eprint(f'Step {step} → {step + 1}: {transform_name}')

        # Apply transform
        result = transform['fn'](source, root_dir=root_dir)

        # Write output
        next_step = step + 1
        out_ext = extension_for(transform['output_type'])
        out_file = f'{routine}.{next_step:02d}{out_ext}'
        out_path = os.path.join(pipeline_dir, out_file)

        with open(out_path, 'w') as f:
            f.write(result)

        eprint(f'  Written: {out_path}')

        # Update manifest
        manifest['steps'].append({
            'step': next_step,
            'file': out_file,
            'transform': transform_name,
        })
        save_manifest(pipeline_dir, manifest)

def cmd_verify(pipeline_dir, step):
    """Verify a pipeline step against reference fixture."""
    routine = get_routine_from_dir(pipeline_dir)
    manifest = load_manifest(pipeline_dir)
    package = manifest.get('package')
    root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    # Find the file for this step
    candidates = glob.glob(os.path.join(pipeline_dir, f'{routine}.{step:02d}.*'))
    if not candidates:
        eprint(f"No file found for step {step}")
        sys.exit(1)

    step_file = candidates[0]
    ext = os.path.splitext(step_file)[1]

    if ext in ('.f', '.f90'):
        # Fortran verification: compile and diff against fixture
        harness = os.path.join(root_dir, 'test', 'run_fortran.sh')
        verify_out = os.path.join(pipeline_dir, f'{routine}.{step:02d}.verify.jsonl')
        fixture = os.path.join(root_dir, 'test', 'fixtures', f'{routine}.jsonl')

        if not os.path.exists(fixture):
            eprint(f"No reference fixture: {fixture}")
            sys.exit(1)

        subprocess.run([
            harness, package, routine,
            '--source', step_file,
            '--output', verify_out,
        ], check=True)

        # Diff
        with open(fixture) as f:
            expected = f.read()
        with open(verify_out) as f:
            actual = f.read()

        if expected == actual:
            eprint(f'VERIFY step {step}: PASS')
        else:
            eprint(f'VERIFY step {step}: FAIL')
            sys.exit(1)

    elif ext == '.js':
        # JS verification: run node --test
        test_file = os.path.join(root_dir, 'lib', package, f'{routine}.test.js')
        if not os.path.exists(test_file):
            eprint(f"No JS test: {test_file}")
            sys.exit(1)

        # Copy to lib for testing
        lib_file = os.path.join(root_dir, 'lib', package, f'{routine}.js')
        shutil.copy2(step_file, lib_file)

        result = subprocess.run(['node', '--test', test_file])
        if result.returncode == 0:
            eprint(f'VERIFY step {step}: PASS')
        else:
            eprint(f'VERIFY step {step}: FAIL')
            sys.exit(1)
    else:
        eprint(f"Unknown file type: {ext}")
        sys.exit(1)

def cmd_list():
    """List available transforms."""
    print(f'{"Name":<30} {"Input→Output":<15} Description')
    print(f'{"─"*30} {"─"*15} {"─"*40}')
    for name, info in sorted(TRANSFORMS.items()):
        io = f'{info["input_type"]}→{info["output_type"]}'
        print(f'{name:<30} {io:<15} {info["description"]}')

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Composable code-mod pipeline for Fortran→JS translation',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('pipeline_dir', nargs='?', help='Pipeline directory (e.g. pipeline/dpotf2)')
    parser.add_argument('--init', nargs=2, metavar=('PACKAGE', 'ROUTINE'),
                        help='Initialize pipeline from source')
    parser.add_argument('--apply', action='append', metavar='TRANSFORM',
                        help='Apply a transform (repeatable)')
    parser.add_argument('--config', metavar='PATH',
                        help='Apply transforms from JSON config file')
    parser.add_argument('--verify', type=int, metavar='STEP',
                        help='Verify a step against reference fixture')
    parser.add_argument('--list', action='store_true',
                        help='List available transforms')
    parser.add_argument('--from', type=int, dest='from_step', metavar='STEP',
                        help='Start applying from a specific step number')

    args = parser.parse_args()

    if args.list:
        cmd_list()
        sys.exit(0)

    if not args.pipeline_dir:
        parser.print_help()
        sys.exit(1)

    if args.init:
        package, routine = args.init
        cmd_init(args.pipeline_dir, package, routine)
    elif args.verify is not None:
        cmd_verify(args.pipeline_dir, args.verify)
    elif args.config:
        with open(args.config) as f:
            config = json.load(f)
        cmd_apply(args.pipeline_dir, config['transformations'], from_step=args.from_step)
    elif args.apply:
        cmd_apply(args.pipeline_dir, args.apply, from_step=args.from_step)
    else:
        parser.print_help()
