

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} s - s
* @param {integer} strideS - strideS
* @param {number} scond - scond
* @param {number} amax - amax
* @returns {*} result
*/
function dlaqsy( uplo, N, A, LDA, s, strideS, scond, amax ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var os;

	sa1 = 1;
	sa2 = LDA;
	os = stride2offset( N, strideS );
	return base( uplo, N, A, sa1, sa2, 0, s, strideS, os, scond, amax );
}


// EXPORTS //

module.exports = dlaqsy;
