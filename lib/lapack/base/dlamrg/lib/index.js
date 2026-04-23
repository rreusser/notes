
'use strict';

/**
* LAPACK dlamrg routine.
*
* @module @stdlib/lapack/base/dlamrg
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
