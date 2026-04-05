
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes a contribution to the reciprocal Dif-estimate using the LU factorization of the n-by-n matrix Z computed by zgetc2.
*
* @param {integer} ijob - method flag: 2 uses zgecon approximation; otherwise local look-ahead
* @param {NonNegativeInteger} N - order of the matrix Z
* @param {Complex128Array} Z - LU-factored N-by-N matrix from zgetc2
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Complex128Array} RHS - right-hand side vector (overwritten with solution)
* @param {integer} strideRHS - stride for `RHS` (in complex elements)
* @param {number} rdsum - input sum of squares contribution
* @param {number} rdscal - input scaling factor
* @param {Int32Array} IPIV - row pivot indices from zgetc2, 0-based
* @param {integer} strideIPIV - stride for `IPIV`
* @param {Int32Array} JPIV - column pivot indices from zgetc2, 0-based
* @param {integer} strideJPIV - stride for `JPIV`
* @returns {Object} object with `rdsum` and `rdscal` properties
*/
function zlatdf( ijob, N, Z, LDZ, RHS, strideRHS, rdsum, rdscal, IPIV, strideIPIV, JPIV, strideJPIV ) { // eslint-disable-line max-len, max-params
	var oipiv;
	var ojpiv;
	var orhs;
	var sz1;
	var sz2;

	sz1 = 1;
	sz2 = LDZ;
	orhs = stride2offset( N, strideRHS );
	oipiv = stride2offset( N, strideIPIV );
	ojpiv = stride2offset( N, strideJPIV );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( ijob, N, Z, sz1, sz2, 0, RHS, strideRHS, orhs, rdsum, rdscal, IPIV, strideIPIV, oipiv, JPIV, strideJPIV, ojpiv ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlatdf;
