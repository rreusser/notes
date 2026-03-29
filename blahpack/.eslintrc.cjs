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
	)
};
