
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the Sturm count for a tridiagonal matrix.
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} LLD - output array
* @param {integer} strideLLD - stride length for `LLD`
* @param {NonNegativeInteger} offsetLLD - starting index for `LLD`
* @param {number} sigma - sigma
* @param {number} pivmin - pivmin
* @param {integer} r - r
* @returns {number} result
*/
function dlaneg( N, d, strideD, offsetD, LLD, strideLLD, offsetLLD, sigma, pivmin, r ) { // eslint-disable-line max-len, max-params
	return base( N, d, strideD, offsetD, LLD, strideLLD, offsetLLD, sigma, pivmin, r ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaneg;
