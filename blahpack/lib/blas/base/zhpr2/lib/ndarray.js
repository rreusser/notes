

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform the Hermitian packed rank-2 update A := alpha*x*y**H + conj(alpha)*y*x**H + A.
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Complex128} alpha - scalar constant
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} y - input array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {Float64Array} AP - output array
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
*/
function zhpr2( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, AP, strideAP, offsetAP ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, AP, strideAP, offsetAP ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhpr2;
