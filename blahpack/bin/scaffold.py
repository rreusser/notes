#!/usr/bin/env python

"""
Generate the stdlib-js module scaffold for a BLAS/LAPACK routine.

Usage:
  python bin/scaffold.py blas daxpy
  python bin/scaffold.py lapack dpotf2
  python bin/scaffold.py lapack dpotf2 --dry-run

Generates all boilerplate files that do NOT depend on the algorithm:
  - package.json
  - lib/index.js
  - lib/main.js
  - lib/<routine>.js (BLAS-style wrapper — stub for LAPACK)
  - lib/ndarray.js (validation wrapper — stub body, correct signature)
  - lib/base.js (stub with correct signature)
  - test/test.js (scaffold)
  - README.md (scaffold)
  - docs/repl.txt (scaffold)
  - docs/types/index.d.ts (scaffold)
  - examples/index.js (scaffold)

The base.js and ndarray.js stubs have the correct function signature
(from signature.py) but a TODO body. The human/AI fills in the algorithm.
"""

import os
import sys
import json
import argparse
import textwrap

# Add bin/ to path so we can import signature
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from signature import generate_signature

YEAR = '2025'
# License headers omitted during development to reduce context/token cost.
# Run bin/add_licenses.sh before contributing to stdlib to add them back.
LICENSE_HEADER = ""


def indent(text, prefix='\t'):
    return '\n'.join(prefix + line if line.strip() else '' for line in text.splitlines())


def gen_base_js(routine, sig, description):
    """Generate lib/base.js — core algorithm stub with correct signature."""
    args = ', '.join(sig['js_args'])
    eslint = ' // eslint-disable-line max-len, max-params' if len(sig['js_args']) > 6 else ''
    returns_line = gen_jsdoc_returns(sig)
    returns_doc = f'\n{returns_line}' if returns_line else ''
    return f"""{LICENSE_HEADER}

'use strict';

// MAIN //

/**
* {description}
*
* @private
{gen_jsdoc_params(sig)}{returns_doc}
*/
function {routine}( {args} ) {{{eslint}
\t// TODO: implement
\tthrow new Error( 'not yet implemented' );
}}


// EXPORTS //

module.exports = {routine};
"""


def gen_ndarray_js(routine, sig, package, description):
    """Generate lib/ndarray.js — validation wrapper with string param checks."""
    args = ', '.join(sig['js_args'])
    base_args = ', '.join(sig['js_args'])
    eslint = ' // eslint-disable-line max-len, max-params' if len(sig['js_args']) > 6 else ''
    eslint_call = ' // eslint-disable-line max-len' if len(sig['js_args']) > 6 else ''
    returns_line = gen_jsdoc_returns(sig)
    returns_doc = f'\n{returns_line}' if returns_line else ''

    # Determine which string params need validation
    STDLIB_VALIDATORS = {
        'uplo': ('isMatrixTriangle', '@stdlib/blas/base/assert/is-matrix-triangle', 'a valid matrix triangle'),
        'trans': ('isTransposeOperation', '@stdlib/blas/base/assert/is-transpose-operation', 'a valid transpose operation'),
        'transa': ('isTransposeOperation', '@stdlib/blas/base/assert/is-transpose-operation', 'a valid transpose operation'),
        'transb': ('isTransposeOperation', '@stdlib/blas/base/assert/is-transpose-operation', 'a valid transpose operation'),
        'diag': ('isDiagonalType', '@stdlib/blas/base/assert/is-diagonal-type', 'a valid diagonal type'),
        'side': ('isOperationSide', '@stdlib/blas/base/assert/is-operation-side', 'a valid operation side'),
    }
    ORDINALS = ['First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth',
                'Seventh', 'Eighth', 'Ninth', 'Tenth', 'Eleventh', 'Twelfth']

    string_params = []
    for i, meta in enumerate(sig.get('js_arg_meta', [])):
        if meta['type'] == 'string' and meta['name'] in STDLIB_VALIDATORS:
            var_name, require_path, err_desc = STDLIB_VALIDATORS[meta['name']]
            ordinal = ORDINALS[i] if i < len(ORDINALS) else f'{i+1}th'
            string_params.append({
                'name': meta['name'],
                'var': var_name,
                'require': require_path,
                'desc': err_desc,
                'ordinal': ordinal,
            })

    # Build requires
    requires = []
    seen_vars = set()
    for sp in string_params:
        if sp['var'] not in seen_vars:
            requires.append(f"var {sp['var']} = require( '{sp['require']}' );")
            seen_vars.add(sp['var'])
    if string_params:
        requires.append("var format = require( '@stdlib/string/format' );")
    requires.append("var base = require( './base.js' );")

    requires_block = '\n'.join(requires)

    # Build validation code
    validation_lines = []
    for sp in string_params:
        validation_lines.append(f"\tif ( !{sp['var']}( {sp['name']} ) ) {{")
        validation_lines.append(f"\t\tthrow new TypeError( format( 'invalid argument. {sp['ordinal']} argument must be {sp['desc']}. Value: `%s`.', {sp['name']} ) );")
        validation_lines.append('\t}')
    validation_block = '\n'.join(validation_lines)

    # Build @throws JSDoc
    throws_lines = []
    for sp in string_params:
        throws_lines.append(f'* @throws {{TypeError}} {sp["ordinal"]} argument must be {sp["desc"]}')
    throws_block = '\n'.join(throws_lines)
    if throws_block:
        throws_block = '\n' + throws_block

    body = validation_block + '\n' if validation_block else ''
    body += f'\treturn base( {base_args} );{eslint_call}'

    return f"""{LICENSE_HEADER}

'use strict';

// MODULES //

{requires_block}


// MAIN //

/**
* {description}
*
{gen_jsdoc_params(sig)}{throws_block}{returns_doc}
*/
function {routine}( {args} ) {{{eslint}
{body}
}}


// EXPORTS //

module.exports = {routine};
"""


def gen_routine_js(routine, sig, package):
    """Generate lib/<routine>.js — BLAS/LAPACK-style API wrapper stub."""
    return f"""{LICENSE_HEADER}

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* TODO: Add BLAS/LAPACK-style API wrapper (order/layout param, LDA instead of strides).
*/
function {routine}() {{
\t// TODO: implement BLAS/LAPACK-style API
\tthrow new Error( 'not yet implemented' );
}}


// EXPORTS //

module.exports = {routine};
"""


def gen_main_js(routine):
    """Generate lib/main.js — attaches .ndarray to the routine."""
    return f"""{LICENSE_HEADER}

'use strict';

// MODULES //

var {routine} = require( './{routine}.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.
// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( {routine}, 'ndarray', {{
\tvalue: ndarray,
\tenumerable: false,
\twritable: false,
\tconfigurable: false
}});


// EXPORTS //

module.exports = {routine};
"""


def gen_index_js(routine, package, description):
    """Generate lib/index.js — entry point."""
    mod_path = f'@stdlib/{package}/base/{routine}'
    return f"""{LICENSE_HEADER}

'use strict';

/**
* {description}
*
* @module {mod_path}
*
* @example
* // TODO: Add example
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: {{ "ndarray": "{routine}.ndarray" }}
"""


def gen_package_json(routine, package, description):
    """Generate package.json."""
    pkg = {
        "name": f"@stdlib/{package}/base/{routine}",
        "version": "0.0.0",
        "description": description,
        "license": "Apache-2.0",
        "author": {
            "name": "The Stdlib Authors",
            "url": "https://github.com/stdlib-js/stdlib/graphs/contributors"
        },
        "contributors": [{
            "name": "The Stdlib Authors",
            "url": "https://github.com/stdlib-js/stdlib/graphs/contributors"
        }],
        "main": "./lib/index.js",
        "directories": {
            "doc": "./docs",
            "example": "./examples",
            "lib": "./lib",
            "test": "./test"
        },
        "scripts": {
            "test": "node --test test/test.js"
        }
    }
    return json.dumps(pkg, indent=2) + '\n'


def gen_test_js(routine, package, sig):
    """Generate test/test.js — scaffold."""
    return f"""{LICENSE_HEADER}

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var {routine} = require( './../lib' );

test( '{routine}: main export is a function', function t() {{
\tassert.strictEqual( typeof {routine}, 'function' );
}});

test( '{routine}: attached to the main export is an `ndarray` method', function t() {{
\tassert.strictEqual( typeof {routine}.ndarray, 'function' );
}});

test( '{routine}: TODO — implement real tests', function t() {{
\tassert.fail( 'Scaffold only — implement real tests before marking complete' );
}});
"""


def gen_readme(routine, package, description, sig):
    """Generate README.md — stdlib-style scaffold."""
    mod_path = f'@stdlib/{package}/base/{routine}'
    ndarray_args = ', '.join(sig['js_args'])

    # Generate parameter list for ndarray section
    param_lines = []
    for meta in sig.get('js_arg_meta', []):
        typed = meta['type']
        if typed in ('Float64Array', 'Float32Array', 'Int32Array'):
            typed = f'[`{typed}`][mdn-{typed.lower()}]'
        param_lines.append(f'-   **{meta["name"]}**: {meta["desc"]}.')

    params_str = '\n'.join(param_lines)

    return f"""# {routine}

> {description}

<section class="usage">

## Usage

```javascript
var {routine} = require( '{mod_path}' );
```

#### {routine}.ndarray( {ndarray_args} )

{description}

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

{params_str}

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   TODO: Add notes.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
// TODO: Add examples
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
"""


def gen_repl_txt(routine, sig, description):
    """Generate docs/repl.txt — stdlib-style scaffold."""
    args = ', '.join(sig['js_args'])

    # Generate parameter docs
    param_lines = []
    for meta in sig.get('js_arg_meta', []):
        param_lines.append(f'        {meta["name"]}: {meta["type"]}')
        param_lines.append(f'            {meta["desc"]}.')
        param_lines.append('')

    params_str = '\n'.join(param_lines)

    returns_type = sig.get('returns_jsdoc', 'void')

    return f"""
    {routine}.ndarray( {args} )
        {description}

        Parameters
        ----------
{params_str}
        Returns
        -------
        out: {returns_type}
            TODO: describe return value.

        Examples
        --------
        // TODO: Add REPL examples

    See Also
    --------

"""


def gen_types_dts(routine, package, sig, description):
    """Generate docs/types/index.d.ts — stdlib-style scaffold."""
    mod_path = f'@stdlib/{package}/base/{routine}'

    # Generate TypeScript parameter types
    ts_params = []
    for meta in sig.get('js_arg_meta', []):
        ts_type_map = {
            'integer': 'number',
            'NonNegativeInteger': 'number',
            'PositiveInteger': 'number',
            'number': 'number',
            'string': 'string',
            'boolean': 'boolean',
            'Float64Array': 'Float64Array',
            'Float32Array': 'Float32Array',
            'Int32Array': 'Int32Array',
        }
        ts_type = ts_type_map.get(meta['type'], 'any')
        ts_params.append(f'\t\t{meta["name"]}: {ts_type}')

    ts_params_str = ',\n'.join(ts_params)

    returns_ts = 'number' if sig.get('returns_jsdoc') else 'void'
    # Arrays return the output array
    for meta in sig.get('js_arg_meta', []):
        if meta['type'] in ('Float64Array', 'Float32Array'):
            returns_ts = meta['type']
            break

    return f"""{LICENSE_HEADER}

// TypeScript declarations for {mod_path}

/**
* Interface describing the ndarray API.
*/
interface Routine {{
\t/**
\t* {description}
\t*/
\t(\n{ts_params_str}\n\t): {returns_ts};
}}

/**
* {description}
*/
declare var {routine}: Routine;

export = {routine};
"""


def gen_examples_js(routine, package):
    """Generate examples/index.js — scaffold."""
    mod_path = f'@stdlib/{package}/base/{routine}'
    return f"""{LICENSE_HEADER}

'use strict';

var {routine} = require( '{mod_path}' );

// TODO: Add example usage
console.log( {routine} );
"""


def gen_learnings_md(routine):
    """Generate LEARNINGS.md template, prefix-aware."""
    is_complex = routine.startswith(('z', 'c'))
    complex_section = (
        '## Complex number handling\n\n'
        '- [ ] (subtleties in complex arithmetic, what was inlined vs library calls)\n'
    ) if is_complex else (
        '## Complex number handling\n\n'
        f'- N/A: {routine} is a real-valued routine.\n'
    )
    return (
        f'# {routine}: Translation Learnings\n\n'
        'TODO: Fill in after implementing base.js. This file is MANDATORY.\n\n'
        '## Translation pitfalls\n\n'
        '- [ ] (describe any index off-by-ones, stride confusion, etc.)\n\n'
        '## Dependency interface surprises\n\n'
        '- [ ] (note unexpected calling conventions of deps)\n\n'
        '## Automation opportunities\n\n'
        '- [ ] (mechanical steps that should be automated)\n\n'
        '## Coverage gaps\n\n'
        '- [ ] (code paths that were hard to test and why)\n\n'
        + complex_section
    )


def gen_jsdoc_params(sig):
    """Generate JSDoc @param lines from signature metadata."""
    lines = []
    for meta in sig.get('js_arg_meta', []):
        lines.append(f'* @param {{{meta["type"]}}} {meta["name"]} - {meta["desc"]}')
    return '\n'.join(lines)


def gen_jsdoc_returns(sig):
    """Generate JSDoc @returns line from signature metadata."""
    rj = sig.get('returns_jsdoc')
    if rj is None:
        return ''
    if sig['returns'] == 'integer':
        return f'* @returns {{{rj}}} status code (0 = success)'
    return f'* @returns {{{rj}}} result'


def main():
    parser = argparse.ArgumentParser(description='Generate stdlib-js module scaffold')
    parser.add_argument('package', choices=['blas', 'lapack'], help='Package (blas or lapack)')
    parser.add_argument('routine', help='Routine name (e.g., daxpy, dpotf2)')
    parser.add_argument('--description', '-d', default=None, help='One-line description')
    parser.add_argument('--dry-run', action='store_true', help='Print files without writing')
    args = parser.parse_args()

    root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    # Locate Fortran source
    if args.package == 'blas':
        fortran_path = os.path.join(root_dir, 'data', 'BLAS-3.12.0', f'{args.routine}.f')
    else:
        fortran_path = os.path.join(root_dir, 'data', 'lapack-3.12.0', 'SRC', f'{args.routine}.f')

    if not os.path.exists(fortran_path):
        # Try .f90 extension
        fortran_path_f90 = fortran_path.replace('.f', '.f90')
        if os.path.exists(fortran_path_f90):
            fortran_path = fortran_path_f90
        else:
            print(f'Error: {fortran_path} not found', file=sys.stderr)
            sys.exit(1)

    # Generate signature
    sig = generate_signature(fortran_path)
    if sig is None:
        print(f'Error: could not parse {fortran_path}', file=sys.stderr)
        sys.exit(1)

    description = args.description or f'TODO: Add description for {args.routine.upper()}.'

    # Build output directory
    out_dir = os.path.join(root_dir, 'lib', args.package, 'base', args.routine)

    # Generate all files
    files = {
        'package.json': gen_package_json(args.routine, args.package, description),
        'lib/base.js': gen_base_js(args.routine, sig, description),
        'lib/ndarray.js': gen_ndarray_js(args.routine, sig, args.package, description),
        f'lib/{args.routine}.js': gen_routine_js(args.routine, sig, args.package),
        'lib/main.js': gen_main_js(args.routine),
        'lib/index.js': gen_index_js(args.routine, args.package, description),
        'test/test.js': gen_test_js(args.routine, args.package, sig),
        'README.md': gen_readme(args.routine, args.package, description, sig),
        'docs/repl.txt': gen_repl_txt(args.routine, sig, description),
        'docs/types/index.d.ts': gen_types_dts(args.routine, args.package, sig, description),
        'examples/index.js': gen_examples_js(args.routine, args.package),
        'LEARNINGS.md': gen_learnings_md(args.routine),
    }

    for rel_path, content in files.items():
        full_path = os.path.join(out_dir, rel_path)
        if args.dry_run:
            print(f'=== {rel_path} ===')
            print(content[:200] + ('...' if len(content) > 200 else ''))
            print()
        else:
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            # Don't overwrite files that may have real content
            protected = {'lib/base.js', 'test/test.js', 'LEARNINGS.md'}
            if os.path.exists(full_path) and rel_path in protected:
                print(f'  SKIP (exists): {rel_path}', file=sys.stderr)
                continue
            with open(full_path, 'w') as f:
                f.write(content)
            print(f'  Created: {rel_path}', file=sys.stderr)

    if not args.dry_run:
        print(f'\nScaffold written to {out_dir}', file=sys.stderr)
        print(f'Next: implement lib/base.js, then run: node --test {out_dir}/test/test.js', file=sys.stderr)


if __name__ == '__main__':
    main()
