'use strict';

// Copy-on-write ESLint plugin.
//
// Priority: local rule in tools/eslint/rules/<name>.js first,
// then fall back to @stdlib/_tools/eslint/rules from the stdlib checkout.

var path = require( 'path' );
var fs = require( 'fs' );

// --- stdlib fallback path ---------------------------------------------------

var STDLIB_DIR = '/Users/rreusser/gh/stdlib-js/stdlib';
var STDLIB_NODE_MODULES = path.join( STDLIB_DIR, 'lib', 'node_modules' );

var stdlibRules;

function loadStdlibRules() {
	if ( stdlibRules ) {
		return stdlibRules;
	}
	// Temporarily add stdlib's internal node_modules to the resolve chain
	var origPaths = module.paths.slice();
	module.paths = [ STDLIB_NODE_MODULES ].concat( origPaths );
	try {
		stdlibRules = require( '@stdlib/_tools/eslint/rules' );
	} catch ( err ) {
		console.error( 'Warning: could not load stdlib ESLint rules from', STDLIB_DIR );
		console.error( err.message );
		stdlibRules = {};
	}
	module.paths = origPaths;
	return stdlibRules;
}

// --- local rules ------------------------------------------------------------

var LOCAL_RULES_DIR = path.join( __dirname, 'rules' );

function loadLocalRules() {
	var rules = {};
	if ( !fs.existsSync( LOCAL_RULES_DIR ) ) {
		return rules;
	}
	fs.readdirSync( LOCAL_RULES_DIR ).forEach( function forEach( file ) {
		if ( file.endsWith( '.cjs' ) ) {
			var name = file.replace( /\.cjs$/, '' );
			rules[ name ] = require( path.join( LOCAL_RULES_DIR, file ) );
		}
	});
	return rules;
}

// --- merged plugin ----------------------------------------------------------

var fallback = loadStdlibRules();
var local = loadLocalRules();

// Local rules override stdlib rules of the same name
var merged = {};
var names = Object.keys( fallback );
var i;
for ( i = 0; i < names.length; i++ ) {
	merged[ names[i] ] = fallback[ names[i] ];
}
names = Object.keys( local );
for ( i = 0; i < names.length; i++ ) {
	merged[ names[i] ] = local[ names[i] ];
}

module.exports = {
	rules: merged
};
