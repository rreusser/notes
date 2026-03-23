

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Reduce NB columns of a general matrix in Hessenberg form
*
* @param {NonNegativeInteger} N - number of columns
* @param {NonNegativeInteger} K - number of superdiagonals
* @param {integer} nb - nb
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} tau - input array
* @param {integer} strideTAU - stride length for `tau`
* @param {NonNegativeInteger} offsetTAU - starting index for `tau`
* @param {Float64Array} t - input array
* @param {integer} strideT - stride length for `t`
* @param {NonNegativeInteger} offsetT - starting index for `t`
* @param {integer} ldt - ldt
* @param {Float64Array} y - input array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {integer} ldy - ldy
*/
function dlahr2( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, t, strideT, offsetT, ldt, y, strideY, offsetY, ldy ) { // eslint-disable-line max-len, max-params
	return base( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, t, strideT, offsetT, ldt, y, strideY, offsetY, ldy ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlahr2;
