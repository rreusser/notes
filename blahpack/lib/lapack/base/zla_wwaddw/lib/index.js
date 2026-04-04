

'use strict';

/**
* Add a complex vector into a doubled-single accumulation vector.
*
* @module @stdlib/lapack/base/zla_wwaddw
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

var zla_wwaddw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_wwaddw = main;
} else {
	zla_wwaddw = tmp;
}


// EXPORTS //

module.exports = zla_wwaddw;

// exports: { "ndarray": "zla_wwaddw.ndarray" }
