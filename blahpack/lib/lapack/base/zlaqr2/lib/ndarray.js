

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Complex aggressive early deflation (non-recursive)
*
* @param {boolean} wantt - wantt
* @param {boolean} wantz - wantz
* @param {NonNegativeInteger} N - number of columns
* @param {integer} ktop - ktop
* @param {integer} kbot - kbot
* @param {integer} nw - nw
* @param {Float64Array} H - input matrix
* @param {integer} strideH1 - stride of the first dimension of `H`
* @param {integer} strideH2 - stride of the second dimension of `H`
* @param {NonNegativeInteger} offsetH - starting index for `H`
* @param {integer} iloz - iloz
* @param {integer} ihiz - ihiz
* @param {Float64Array} Z - input matrix
* @param {integer} strideZ1 - stride of the first dimension of `Z`
* @param {integer} strideZ2 - stride of the second dimension of `Z`
* @param {NonNegativeInteger} offsetZ - starting index for `Z`
* @param {integer} ns - ns
* @param {integer} nd - nd
* @param {Float64Array} SH - input array
* @param {integer} strideSH - stride length for `SH`
* @param {NonNegativeInteger} offsetSH - starting index for `SH`
* @param {Float64Array} V - input matrix
* @param {integer} strideV1 - stride of the first dimension of `V`
* @param {integer} strideV2 - stride of the second dimension of `V`
* @param {NonNegativeInteger} offsetV - starting index for `V`
* @param {integer} nh - nh
* @param {Float64Array} T - input matrix
* @param {integer} strideT1 - stride of the first dimension of `T`
* @param {integer} strideT2 - stride of the second dimension of `T`
* @param {NonNegativeInteger} offsetT - starting index for `T`
* @param {integer} nv - nv
* @param {Float64Array} WV - input matrix
* @param {integer} strideWV1 - stride of the first dimension of `WV`
* @param {integer} strideWV2 - stride of the second dimension of `WV`
* @param {NonNegativeInteger} offsetWV - starting index for `WV`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
*/
function zlaqr2( wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, ns, nd, SH, strideSH, offsetSH, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, ns, nd, SH, strideSH, offsetSH, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlaqr2;
