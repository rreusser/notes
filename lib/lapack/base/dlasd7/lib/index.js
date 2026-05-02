'use strict';

/**
* Merge two sets of singular values together into a single sorted set and deflate.
*
* @module @stdlib/lapack/base/dlasd7
*
*
* @example
* var dlasd7 = require( '@stdlib/lapack/base/dlasd7' );
*
* var N = 3;
* var GIVCOL = discreteUniform( N * N, -10, 10, opts );
* var GIVNUM = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var z = discreteUniform( N, -10, 10, opts );
* var ZW = discreteUniform( N, -10, 10, opts );
* var VF = discreteUniform( N, -10, 10, opts );
* var VFW = discreteUniform( N, -10, 10, opts );
* var VL = discreteUniform( N, -10, 10, opts );
* var VLW = discreteUniform( N, -10, 10, opts );
* var DSIGMA = discreteUniform( N, -10, 10, opts );
* var IDX = discreteUniform( N, -10, 10, opts );
* var IDXP = discreteUniform( N, -10, 10, opts );
* var IDXQ = discreteUniform( N, -10, 10, opts );
* var PERM = discreteUniform( N, -10, 10, opts );
*
* dlasd7.ndarray( 1, 1, 1, 1, d, 1, 0, z, 1, 0, ZW, 1, 0, VF, 1, 0, VFW, 1, 0, VL, 1, 0, VLW, 1, 0, 1.0, 1.0, DSIGMA, 1, 0, IDX, 1, 0, IDXP, 1, 0, IDXQ, 1, 0, PERM, 1, 0, GIVCOL, N, 1, 0, GIVNUM, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlasd7;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasd7 = main;
} else {
	dlasd7 = tmp;
}


// EXPORTS //

module.exports = dlasd7;

// exports: { "ndarray": "dlasd7.ndarray" }
