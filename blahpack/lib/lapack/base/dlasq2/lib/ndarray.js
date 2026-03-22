

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute all eigenvalues of a symmetric positive definite tridiagonal matrix via dqds
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} z - input array
* @param {integer} stride - stride length for `z`
* @param {NonNegativeInteger} offset - starting index for `z`
* @returns {integer} status code (0 = success)
*/
function dlasq2( N, z, stride, offset ) {
	return base( N, z, stride, offset );
}


// EXPORTS //

module.exports = dlasq2;
