

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * Compute the number of simultaneous shifts (ISPEC=15) per iparmq.
 *
 *
 * @param {integer} nh - IHI-ILO+1
 * @returns {integer} ns
 */
function dlaqr0( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqr0;
