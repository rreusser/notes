

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {integer} itype - itype
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {*} result
*/
function dsygst( itype, uplo, N, A, LDA, B, LDB ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var sb1;
	var sb2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	return base( itype, uplo, N, A, sa1, sa2, 0, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = dsygst;
