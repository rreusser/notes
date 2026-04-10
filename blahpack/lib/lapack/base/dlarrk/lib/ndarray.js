
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes one eigenvalue of a symmetric tridiagonal matrix to suitable accuracy via bisection.
*
* @param {NonNegativeInteger} N - order of the matrix
* @param {integer} iw - 1-based index of the eigenvalue to compute
* @param {number} gl - lower bound of the initial interval
* @param {number} gu - upper bound of the initial interval
* @param {Float64Array} D - diagonal elements (length `N`)
* @param {integer} strideD - stride length for `D`
* @param {NonNegativeInteger} offsetD - starting index for `D`
* @param {Float64Array} E2 - squared off-diagonal elements (length `N-1`)
* @param {integer} strideE2 - stride length for `E2`
* @param {NonNegativeInteger} offsetE2 - starting index for `E2`
* @param {number} pivmin - minimum pivot in the Sturm sequence
* @param {number} reltol - relative tolerance for the returned eigenvalue
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {Object} object with `info`, `w`, and `werr` properties
*/
function dlarrk( N, iw, gl, gu, D, strideD, offsetD, E2, strideE2, offsetE2, pivmin, reltol ) { // eslint-disable-line max-len, max-params
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, iw, gl, gu, D, strideD, offsetD, E2, strideE2, offsetE2, pivmin, reltol );
}


// EXPORTS //

module.exports = dlarrk;
