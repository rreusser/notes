
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes all eigenvalues and, optionally, eigenvectors of a real symmetric.
*
* @param {string} jobz - jobz
* @param {NonNegativeInteger} N - N
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @param {Float64Array} Z - Z
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function dstev( jobz, N, d, strideD, e, strideE, Z, LDZ, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var sz1;
	var sz2;
	var od;
	var oe;

	sz1 = 1;
	sz2 = LDZ;
	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	owork = stride2offset( N, strideWORK );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( jobz, N, d, strideD, od, e, strideE, oe, Z, sz1, sz2, 0, WORK, strideWORK, owork );
}


// EXPORTS //

module.exports = dstev;
