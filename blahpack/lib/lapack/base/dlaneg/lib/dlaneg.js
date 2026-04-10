
/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the Sturm count for a tridiagonal matrix.
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {Float64Array} LLD - output array
* @param {integer} strideLLD - stride length for `LLD`
* @param {number} sigma - sigma
* @param {number} pivmin - pivmin
* @param {integer} r - r
* @returns {number} result
*/
function dlaneg( N, d, strideD, LLD, strideLLD, sigma, pivmin, r ) { // eslint-disable-line max-len, max-params
	var oLLD = stride2offset( N, strideLLD );
	var od = stride2offset( N, strideD );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, d, strideD, od, LLD, strideLLD, oLLD, sigma, pivmin, r ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaneg;
