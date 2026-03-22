

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the Euclidean norm of a real vector.
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @returns {number} result
*/
function dnrm2( N, x, stride, offset ) {
	return base( N, x, stride, offset );
}


// EXPORTS //

module.exports = dnrm2;
