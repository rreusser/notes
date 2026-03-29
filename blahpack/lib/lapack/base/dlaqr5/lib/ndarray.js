
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Accesses a 2D array element (1-based row i, column j).
*
* @param {Float64Array} A - array
* @param {integer} sA1 - row stride
* @param {integer} sA2 - column stride
* @param {integer} oA - offset
* @param {integer} i - 1-based row
* @param {integer} j - 1-based column
* @returns {number} element value
*/
function dlaqr5( wantt, wantz, kacc22, N, ktop, kbot, nshfts, SR, strideSR, offsetSR, SI, strideSI, offsetSI, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, V, strideV1, strideV2, offsetV, U, strideU1, strideU2, offsetU, nv, WV, strideWV1, strideWV2, offsetWV, nh, WH, strideWH1, strideWH2, offsetWH ) { // eslint-disable-line max-len, max-params
	return base( wantt, wantz, kacc22, N, ktop, kbot, nshfts, SR, strideSR, offsetSR, SI, strideSI, offsetSI, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, V, strideV1, strideV2, offsetV, U, strideU1, strideU2, offsetU, nv, WV, strideWV1, strideWV2, offsetWV, nh, WH, strideWH1, strideWH2, offsetWH ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqr5;
