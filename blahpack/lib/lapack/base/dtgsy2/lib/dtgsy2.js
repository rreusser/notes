
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} trans - trans
* @param {integer} ijob - ijob
* @param {PositiveInteger} M - M
* @param {PositiveInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} C - C
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Float64Array} D - D
* @param {PositiveInteger} LDD - leading dimension of `D`
* @param {Float64Array} E - E
* @param {PositiveInteger} LDE - leading dimension of `E`
* @param {Float64Array} F - F
* @param {PositiveInteger} LDF - leading dimension of `F`
* @param {Float64Array} scale - scale
* @param {Float64Array} rdsum - rdsum
* @param {Float64Array} rdscal - rdscal
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @param {Int32Array} pq - pq
* @returns {*} result
*/
function dtgsy2( trans, ijob, M, N, A, LDA, B, LDB, C, LDC, D, LDD, E, LDE, F, LDF, scale, rdsum, rdscal, IWORK, strideIWORK, pq ) { // eslint-disable-line max-len, max-params
	var oiwork;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var sc1;
	var sc2;
	var sd1;
	var sd2;
	var se1;
	var se2;
	var sf1;
	var sf2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	sc1 = 1;
	sc2 = LDC;
	sd1 = 1;
	sd2 = LDD;
	se1 = 1;
	se2 = LDE;
	sf1 = 1;
	sf2 = LDF;
	oiwork = stride2offset( N, strideIWORK );
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( trans, ijob, M, N, A, sa1, sa2, 0, B, sb1, sb2, 0, C, sc1, sc2, 0, D, sd1, sd2, 0, E, se1, se2, 0, F, sf1, sf2, 0, scale, rdsum, rdscal, IWORK, strideIWORK, oiwork, pq ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtgsy2;
