'use strict';

var path = require( 'path' );
var configDir = path.join( __dirname, 'tools', 'eslint', 'config' );

function loadConfig( name ) {
	return require( path.join( configDir, name + '.json' ) );
}

module.exports = {
	root: true,

	plugins: [ 'stdlib', 'node' ],

	env: {
		browser: true,
		node: true,
		commonjs: true,
		es6: true
	},

	parserOptions: {
		ecmaVersion: 6,
		sourceType: 'script',
		ecmaFeatures: {
			globalReturn: false,
			impliedStrict: false,
			jsx: false
		}
	},

	rules: Object.assign(
		{},
		loadConfig( 'programmer_errors' ),
		loadConfig( 'best_practices' ),
		loadConfig( 'strict' ),
		loadConfig( 'variables' ),
		loadConfig( 'nodejs' ),
		loadConfig( 'style' ),
		loadConfig( 'es2015' ),
		loadConfig( 'stdlib' ),
		loadConfig( 'blahpack' )
	),

	// The stdlib/doctest and stdlib/jsdoc-doctest rules use vm.runInContext
	// to execute the full source text so that @example return-annotations can
	// be validated. On test files this registers node:test cases in the host
	// realm, which then run on process beforeExit and fail with cross-realm
	// TypeError/RangeError mismatches. Test files have no doctests to
	// validate, so disable both rules on test/benchmark files.
	overrides: [
		{
			files: [ 'lib/**/test/**/*.js', 'lib/**/benchmark/**/*.js', 'lib/**/examples/**/*.js' ],
			rules: {
				'stdlib/doctest': 'off',
				'stdlib/jsdoc-doctest': 'off'
			}
		}
	]
};
