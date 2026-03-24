

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a tridiagonal system factored by dlagtf
*
* @param {integer} job - job
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} a - input array
* @param {integer} strideA - stride length for `a`
* @param {NonNegativeInteger} offsetA - starting index for `a`
* @param {Float64Array} b - input array
* @param {integer} strideB - stride length for `b`
* @param {NonNegativeInteger} offsetB - starting index for `b`
* @param {Float64Array} c - input array
* @param {integer} strideC - stride length for `c`
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Int32Array} IN - input array
* @param {integer} strideIN - stride length for `IN`
* @param {NonNegativeInteger} offsetIN - starting index for `IN`
* @param {Float64Array} y - output array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {number} tol - tol
* @returns {integer} status code (0 = success)
*/
function dlagts( job, N, a, strideA, offsetA, b, strideB, offsetB, c, strideC, offsetC, d, strideD, offsetD, IN, strideIN, offsetIN, y, strideY, offsetY, tol ) { // eslint-disable-line max-len, max-params
	return base( job, N, a, strideA, offsetA, b, strideB, offsetB, c, strideC, offsetC, d, strideD, offsetD, IN, strideIN, offsetIN, y, strideY, offsetY, tol ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlagts;
