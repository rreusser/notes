

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Generates a vector of random numbers from a specified distribution
*
* @param {integer} idist - idist
* @param {Int32Array} iseed - input array
* @param {integer} strideISEED - stride length for `iseed`
* @param {NonNegativeInteger} offsetISEED - starting index for `iseed`
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
*/
function dlarnv( idist, iseed, strideISEED, offsetISEED, N, x, stride, offset ) { // eslint-disable-line max-len, max-params
	return base( idist, iseed, strideISEED, offsetISEED, N, x, stride, offset ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlarnv;
