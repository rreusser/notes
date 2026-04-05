
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} RHS - RHS
* @param {integer} strideRHS - strideRHS
* @param {Int32Array} IPIV - IPIV
* @param {integer} strideIPIV - strideIPIV
* @param {Int32Array} JPIV - JPIV
* @param {integer} strideJPIV - strideJPIV
* @param {Float64Array} scale - scale
* @returns {*} result
*/
function dgesc2( N, A, LDA, RHS, strideRHS, IPIV, strideIPIV, JPIV, strideJPIV, scale ) { // eslint-disable-line max-len, max-params
	var oipiv;
	var ojpiv;
	var orhs;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	orhs = stride2offset( N, strideRHS );
	oipiv = stride2offset( N, strideIPIV );
	ojpiv = stride2offset( N, strideJPIV );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, A, sa1, sa2, 0, RHS, strideRHS, orhs, IPIV, strideIPIV, oipiv, JPIV, strideJPIV, ojpiv, scale ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgesc2;
