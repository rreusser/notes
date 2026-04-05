

'use strict';

// MODULES //

var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
 * CABS1: |re(z)| + |im(z)|
 *
 *
 * @param {Float64Array} v - Float64 view
 * @param {integer} idx - index of real part
 * @throws {TypeError} First argument must be a valid operation side
 * @returns {number} CABS1 value
 */
function ztrevc3( side, howmny, SELECT, strideSELECT, offsetSELECT, N, T, strideT1, strideT2, offsetT, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, mm, M, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK, lrwork ) { // eslint-disable-line max-len, max-params
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Twentieth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	return base( side, howmny, SELECT, strideSELECT, offsetSELECT, N, T, strideT1, strideT2, offsetT, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, mm, M, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK, lrwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrevc3;
