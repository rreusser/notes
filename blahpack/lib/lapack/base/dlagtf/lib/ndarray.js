

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Factorizes the matrix (T - lambda*I) where T is a tridiagonal matrix
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} a - input array
* @param {integer} strideA - stride length for `a`
* @param {NonNegativeInteger} offsetA - starting index for `a`
* @param {number} lambda - lambda
* @param {Float64Array} b - input array
* @param {integer} strideB - stride length for `b`
* @param {NonNegativeInteger} offsetB - starting index for `b`
* @param {Float64Array} c - input array
* @param {integer} strideC - stride length for `c`
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {number} tol - tol
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Int32Array} IN - output array
* @param {integer} strideIN - stride length for `IN`
* @param {NonNegativeInteger} offsetIN - starting index for `IN`
* @returns {integer} status code (0 = success)
*/
function dlagtf( N, a, strideA, offsetA, lambda, b, strideB, offsetB, c, strideC, offsetC, tol, d, strideD, offsetD, IN, strideIN, offsetIN ) { // eslint-disable-line max-len, max-params
	return base( N, a, strideA, offsetA, lambda, b, strideB, offsetB, c, strideC, offsetC, tol, d, strideD, offsetD, IN, strideIN, offsetIN ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlagtf;
