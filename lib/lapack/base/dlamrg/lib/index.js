
'use strict';

/**
* LAPACK dlamrg routine.
*
* @module @stdlib/lapack/base/dlamrg
*
*
* @example
* var dlamrg = require( '@stdlib/lapack/base/dlamrg' );
*
* var a = discreteUniform( N, -10, 10, opts );
* var INDEX = discreteUniform( N, -10, 10, opts );
*
* dlamrg.ndarray( 1, 1, a, 1, 0, 1, 1, INDEX, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlamrg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlamrg = main;
} else {
	dlamrg = tmp;
}


// EXPORTS //

module.exports = dlamrg;

// exports: { "ndarray": "dlamrg.ndarray" }
