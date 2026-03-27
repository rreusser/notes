

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {string} trans - trans
* @param {string} diag - diag
* @param {string} normin - normin
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} kd - kd
* @param {Float64Array} AB - AB
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} x - x
* @param {integer} strideX - strideX
* @param {Float64Array} scale - scale
* @param {Float64Array} CNORM - CNORM
* @param {integer} strideCNORM - strideCNORM
* @returns {*} result
*/
function dlatbs( uplo, trans, diag, normin, N, kd, AB, LDAB, x, strideX, scale, CNORM, strideCNORM ) { // eslint-disable-line max-len, max-params
	var ocnorm;
	var sab1;
	var sab2;
	var ox;

	sab1 = 1;
	sab2 = LDAB;
	ox = stride2offset( N, strideX );
	ocnorm = stride2offset( N, strideCNORM );
	return base( uplo, trans, diag, normin, N, kd, AB, sab1, sab2, 0, x, strideX, ox, scale, CNORM, strideCNORM, ocnorm ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlatbs;
