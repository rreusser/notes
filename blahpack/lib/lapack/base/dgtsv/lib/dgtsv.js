
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a general real tridiagonal system of linear equations A * X = B.
*
* @param {NonNegativeInteger} N - N
* @param {integer} nrhs - nrhs
* @param {Float64Array} DL - DL
* @param {integer} strideDL - strideDL
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} DU - DU
* @param {integer} strideDU - strideDU
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {*} result
*/
function dgtsv( N, nrhs, DL, strideDL, d, strideD, DU, strideDU, B, LDB ) { // eslint-disable-line max-len, max-params
	var odl;
	var odu;
	var sb1;
	var sb2;
	var od;

	sb1 = 1;
	sb2 = LDB;
	odl = stride2offset( N, strideDL );
	od = stride2offset( N, strideD );
	odu = stride2offset( N, strideDU );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( nrhs < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', nrhs ) );
	}
	return base( N, nrhs, DL, strideDL, odl, d, strideD, od, DU, strideDU, odu, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = dgtsv;
