'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Returns the norm of a complex general tridiagonal matrix A.
*
* @param {string} norm - norm
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} DL - DL
* @param {integer} strideDL - strideDL
* @param {Complex128Array} d - d
* @param {integer} strideD - strideD
* @param {Complex128Array} DU - DU
* @param {integer} strideDU - strideDU
* @returns {number} result
*/
function zlangt( norm, N, DL, strideDL, d, strideD, DU, strideDU ) { // eslint-disable-line max-len, max-params
	var odl;
	var odu;
	var od;

	odl = stride2offset( N, strideDL );
	od = stride2offset( N, strideD );
	odu = stride2offset( N, strideDU );
	return base( norm, N, DL, strideDL, odl, d, strideD, od, DU, strideDU, odu ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlangt;
