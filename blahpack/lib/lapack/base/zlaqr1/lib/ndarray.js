

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Set initial vector for Francis QR step
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} H - input matrix
* @param {integer} strideH1 - stride of the first dimension of `H`
* @param {integer} strideH2 - stride of the second dimension of `H`
* @param {NonNegativeInteger} offsetH - starting index for `H`
* @param {Complex128} s1 - s1
* @param {Complex128} s2 - s2
* @param {Float64Array} v - output array
* @param {integer} strideV - stride length for `v`
* @param {NonNegativeInteger} offsetV - starting index for `v`
*/
function zlaqr1( N, H, strideH1, strideH2, offsetH, s1, s2, v, strideV, offsetV ) { // eslint-disable-line max-len, max-params
	return base( N, H, strideH1, strideH2, offsetH, s1, s2, v, strideV, offsetV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlaqr1;
