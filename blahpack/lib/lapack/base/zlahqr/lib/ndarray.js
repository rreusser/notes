

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute eigenvalues and Schur form of upper Hessenberg matrix
*
* @param {boolean} wantt - wantt
* @param {boolean} wantz - wantz
* @param {NonNegativeInteger} N - number of columns
* @param {integer} ilo - ilo
* @param {integer} ihi - ihi
* @param {Float64Array} H - input matrix
* @param {integer} strideH1 - stride of the first dimension of `H`
* @param {integer} strideH2 - stride of the second dimension of `H`
* @param {NonNegativeInteger} offsetH - starting index for `H`
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {integer} iloz - iloz
* @param {integer} ihiz - ihiz
* @param {Float64Array} Z - output matrix
* @param {integer} strideZ1 - stride of the first dimension of `Z`
* @param {integer} strideZ2 - stride of the second dimension of `Z`
* @param {NonNegativeInteger} offsetZ - starting index for `Z`
* @returns {integer} status code (0 = success)
*/
function zlahqr( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, w, strideW, offsetW, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ ) { // eslint-disable-line max-len, max-params
	return base( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, w, strideW, offsetW, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlahqr;
