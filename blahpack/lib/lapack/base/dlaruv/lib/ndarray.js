

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Generates a vector of random numbers from a uniform distribution
*
* @param {Int32Array} iseed - input array
* @param {integer} strideISEED - stride length for `iseed`
* @param {NonNegativeInteger} offsetISEED - starting index for `iseed`
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
*/
function dlaruv( iseed, strideISEED, offsetISEED, N, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	return base( iseed, strideISEED, offsetISEED, N, x, strideX, offsetX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaruv;
