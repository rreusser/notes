
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Returns the norm of a complex general band matrix.
*
* @param {string} norm - norm type
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} KL - number of sub-diagonals
* @param {NonNegativeInteger} KU - number of super-diagonals
* @param {Complex128Array} AB - band matrix in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK
* @returns {number} norm value
*/
function zlangb( norm, N, KL, KU, AB, LDAB, WORK, strideWORK ) { // eslint-disable-line max-params
	var owork;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDAB;
	owork = stride2offset( N, strideWORK );
	return base( norm, N, KL, KU, AB, sa1, sa2, 0, WORK, strideWORK, owork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlangb;
