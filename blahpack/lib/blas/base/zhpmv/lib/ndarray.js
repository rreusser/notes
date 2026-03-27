

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform the Hermitian packed matrix-vector operation y := alpha*A*x + beta*y.
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Complex128} alpha - scalar constant
* @param {Float64Array} AP - input array
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Complex128} beta - scalar constant
* @param {Float64Array} y - output array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
*/
function zhpmv( uplo, N, alpha, AP, strideAP, offsetAP, x, strideX, offsetX, beta, y, strideY, offsetY ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, alpha, AP, strideAP, offsetAP, x, strideX, offsetX, beta, y, strideY, offsetY ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhpmv;
