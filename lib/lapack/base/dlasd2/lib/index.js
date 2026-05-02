
'use strict';

/**
* Merge two sets of singular values in bidiagonal SVD divide and conquer.
*
* @module @stdlib/lapack/base/dlasd2
*
*
* @example
* var dlasd2 = require( '@stdlib/lapack/base/dlasd2' );
*
* var N = 3;
* var U = discreteUniform( N * N, -10, 10, opts );
* var VT = discreteUniform( N * N, -10, 10, opts );
* var U2 = discreteUniform( N * N, -10, 10, opts );
* var VT2 = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var z = discreteUniform( N, -10, 10, opts );
* var DSIGMA = discreteUniform( N, -10, 10, opts );
* var IDXP = discreteUniform( N, -10, 10, opts );
* var IDX = discreteUniform( N, -10, 10, opts );
* var IDXC = discreteUniform( N, -10, 10, opts );
* var IDXQ = discreteUniform( N, -10, 10, opts );
* var COLTYP = discreteUniform( N, -10, 10, opts );
*
* dlasd2.ndarray( 1, 1, 1, N, d, 1, 0, z, 1, 0, 1.0, 1.0, U, N, 1, 0, VT, N, 1, 0, DSIGMA, 1, 0, U2, N, 1, 0, VT2, N, 1, 0, IDXP, 1, 0, IDX, 1, 0, IDXC, 1, 0, IDXQ, 1, 0, COLTYP, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlasd2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasd2 = main;
} else {
	dlasd2 = tmp;
}


// EXPORTS //

module.exports = dlasd2;

// exports: { "ndarray": "dlasd2.ndarray" }
