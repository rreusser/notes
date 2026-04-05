
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Complex128} alpha - alpha
* @param {Complex128Array} AP - AP
* @param {integer} strideAP - strideAP
* @param {Complex128Array} x - x
* @param {integer} strideX - strideX
* @param {Complex128} beta - beta
* @param {Complex128Array} y - y
* @param {integer} strideY - strideY
* @returns {*} result
*/
function zhpmv( uplo, N, alpha, AP, strideAP, x, strideX, beta, y, strideY ) { // eslint-disable-line max-len, max-params
	var oap;
	var ox;
	var oy;

	oap = stride2offset( N, strideAP );
	ox = stride2offset( N, strideX );
	oy = stride2offset( N, strideY );
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( uplo, N, alpha, AP, strideAP, oap, x, strideX, ox, beta, y, strideY, oy );
}


// EXPORTS //

module.exports = zhpmv;
