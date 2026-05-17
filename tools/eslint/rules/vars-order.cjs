'use strict';

// Local override of stdlib/vars-order.
//
// The upstream rule's auto-fixer blindly sorts var declarations inside a
// function body by name length, with no awareness of initializer dependencies.
// On test files like:
//
//     var view = reinterpret(arr, 0);
//     var arr  = new Complex128Array(...);
//
// the upstream fixer reorders to put `view` first (longer name) and produces
// a use-before-define bug. Three translation agents have been bitten by this
// in the same session.
//
// The dep-respecting reorder is handled by `bin/codemod-tests.js` (a
// toposort), which always runs as part of `bin/lint-fix.sh`. So we keep the
// upstream rule's *validation* (so agents still see the warning when vars are
// out of order) but strip the auto-fixer. To actually reorder, run
// `bin/lint-fix.sh <module>` — that goes through the codemod first.
//
// Implementation: load the upstream rule and intercept `context.report` to
// drop the `fix` callback. Validation behavior is unchanged.

var path = require( 'path' );

var STDLIB_DIR = '/Users/rreusser/gh/stdlib-js/stdlib';
var STDLIB_NODE_MODULES = path.join( STDLIB_DIR, 'lib', 'node_modules' );

var origPaths = module.paths.slice();
module.paths = [ STDLIB_NODE_MODULES ].concat( origPaths );

var upstream;
try {
	upstream = require( '@stdlib/_tools/eslint/rules/vars-order' );
} catch ( err ) {
	module.paths = origPaths;
	throw err;
}
module.paths = origPaths;

function create( context ) {
	// Upstream rule only reads context.options, context.sourceCode, and
	// context.report. Build a plain wrapper that strips `fix` from reports.
	var wrapped = {
		options: context.options,
		sourceCode: context.sourceCode,
		report: function report( descriptor ) {
			var stripped = {};
			Object.keys( descriptor ).forEach( function copyKey( k ) {
				if ( k !== 'fix' ) {
					stripped[ k ] = descriptor[ k ];
				}
			});
			return context.report( stripped );
		}
	};
	return upstream.create( wrapped );
}

module.exports = {
	'meta': Object.assign( {}, upstream.meta, { 'fixable': null } ),
	'create': create
};
