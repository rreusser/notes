

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solve one of the triangular packed systems A*x = b or A**T*x = b or A**H*x = b.
*
* @param {string} uplo - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} AP - input array
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
* @param {Float64Array} x - output array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
*/
function ztpsv( uplo, trans, diag, N, AP, strideAP, offsetAP, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	return base( uplo, trans, diag, N, AP, strideAP, offsetAP, x, strideX, offsetX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztpsv;
