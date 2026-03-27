

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform the Hermitian packed rank-1 update A := alpha*x*x**H + A.
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {number} alpha - scalar constant
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} AP - output array
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
*/
function zhpr( uplo, N, alpha, x, strideX, offsetX, AP, strideAP, offsetAP ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, alpha, x, strideX, offsetX, AP, strideAP, offsetAP ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhpr;
