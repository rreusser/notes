

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform one of the triangular packed matrix-vector operations x := A*x or x := A**T*x or x := A**H*x.
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
function ztpmv( uplo, trans, diag, N, AP, strideAP, offsetAP, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	return base( uplo, trans, diag, N, AP, strideAP, offsetAP, x, strideX, offsetX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztpmv;
