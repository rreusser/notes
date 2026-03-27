

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} norm - norm
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function zlansy( norm, uplo, N, A, LDA, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	owork = stride2offset( N, strideWORK );
	return base( norm, uplo, N, A, sa1, sa2, 0, WORK, strideWORK, owork );
}


// EXPORTS //

module.exports = zlansy;
