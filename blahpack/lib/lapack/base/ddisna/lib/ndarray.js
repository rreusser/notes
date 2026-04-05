

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the reciprocal condition numbers for the eigenvectors of a real symmetric or complex Hermitian matrix
*
* @param {string} job - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} SEP - output array
* @param {integer} strideSEP - stride length for `SEP`
* @param {NonNegativeInteger} offsetSEP - starting index for `SEP`
* @returns {integer} status code (0 = success)
*/
function ddisna( job, M, N, d, strideD, offsetD, SEP, strideSEP, offsetSEP ) { // eslint-disable-line max-len, max-params
	return base( job, M, N, d, strideD, offsetD, SEP, strideSEP, offsetSEP ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ddisna;
