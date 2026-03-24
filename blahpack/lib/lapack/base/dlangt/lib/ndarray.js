

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the norm of a general tridiagonal matrix
*
* @param {string} norm - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} DL - input array
* @param {integer} strideDL - stride length for `DL`
* @param {NonNegativeInteger} offsetDL - starting index for `DL`
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} DU - output array
* @param {integer} strideDU - stride length for `DU`
* @param {NonNegativeInteger} offsetDU - starting index for `DU`
* @returns {number} result
*/
function dlangt( norm, N, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU ) { // eslint-disable-line max-len, max-params
	return base( norm, N, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlangt;
