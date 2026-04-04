
'use strict';

/**
* Solves a system of linear equations with an LU factored matrix using complete pivoting.
*
* @module @stdlib/lapack/base/dgesc2
*
* @example
* // TODO: Add example
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgesc2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgesc2 = main;
} else {
	dgesc2 = tmp;
}


// EXPORTS //

module.exports = dgesc2;

// exports: { "ndarray": "dgesc2.ndarray" }
