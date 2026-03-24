

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform symmetric rank-2 update of a packed matrix
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {number} alpha - scalar constant
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
function dspr2( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, AP, strideAP, offsetAP ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, AP, strideAP, offsetAP ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dspr2;
