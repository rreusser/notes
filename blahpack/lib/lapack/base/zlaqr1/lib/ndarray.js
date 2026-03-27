

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * CABS1 = |Re(z)| + |Im(z)|.
 *
 *
 * @param {Complex128Array} v - interleaved view
 * @param {integer} idx - Float64 index
 * @returns {number} result
 */
function zlaqr1( N, H, strideH1, strideH2, offsetH, s1, s2, v, strideV, offsetV ) { // eslint-disable-line max-len, max-params
	return base( N, H, strideH1, strideH2, offsetH, s1, s2, v, strideV, offsetV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlaqr1;
