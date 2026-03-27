

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * CABS1(z) = |Re(z)| + |Im(z)|.
 *
 *
 * @param {Complex128Array} v - interleaved Float64 view
 * @param {integer} idx - Float64 index
 * @returns {number} result
 */
function zlahqr( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, w, strideW, offsetW, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ ) { // eslint-disable-line max-len, max-params
	return base( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, w, strideW, offsetW, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlahqr;
