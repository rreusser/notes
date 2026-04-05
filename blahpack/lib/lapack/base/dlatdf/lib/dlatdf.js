
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {integer} ijob - ijob
* @param {NonNegativeInteger} N - N
* @param {Float64Array} Z - Z
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Float64Array} RHS - RHS
* @param {integer} strideRHS - strideRHS
* @param {number} rdsum - rdsum
* @param {number} rdscal - rdscal
* @param {Int32Array} IPIV - IPIV
* @param {integer} strideIPIV - strideIPIV
* @param {Int32Array} JPIV - JPIV
* @param {integer} strideJPIV - strideJPIV
* @returns {*} result
*/
function dlatdf( ijob, N, Z, LDZ, RHS, strideRHS, rdsum, rdscal, IPIV, strideIPIV, JPIV, strideJPIV ) { // eslint-disable-line max-len, max-params
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

module.exports = dlatdf;
