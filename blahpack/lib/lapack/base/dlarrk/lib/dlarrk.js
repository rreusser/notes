

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes one eigenvalue of a symmetric tridiagonal matrix to suitable accuracy
*
* @param {NonNegativeInteger} N - number of columns
* @param {integer} iw - iw
* @param {number} gl - gl
* @param {number} gu - gu
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {Float64Array} E2 - output array
* @param {integer} strideE2 - stride length for `E2`
* @param {number} pivmin - pivmin
* @param {number} reltol - reltol
* @param {number} w - w
* @param {number} werr - werr
* @returns {integer} status code (0 = success)
*/
function dlarrk( N, iw, gl, gu, d, strideD, E2, strideE2, pivmin, reltol, w, werr ) { // eslint-disable-line max-len, max-params
	var od = stride2offset( N, strideD );
	var oE2 = stride2offset( N, strideE2 );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, iw, gl, gu, d, strideD, od, E2, strideE2, oE2, pivmin, reltol, w, werr ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlarrk;
