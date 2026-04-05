
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {integer} job - job
* @param {NonNegativeInteger} N - N
* @param {Float64Array} a - a
* @param {integer} strideA - strideA
* @param {Float64Array} b - b
* @param {integer} strideB - strideB
* @param {Float64Array} c - c
* @param {integer} strideC - strideC
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Int32Array} IN - IN
* @param {integer} strideIN - strideIN
* @param {Float64Array} y - y
* @param {integer} strideY - strideY
* @param {number} tol - tol
* @returns {*} result
*/
function dlagts( job, N, a, strideA, b, strideB, c, strideC, d, strideD, IN, strideIN, y, strideY, tol ) { // eslint-disable-line max-len, max-params
	var oin;
	var oa;
	var ob;
	var oc;
	var od;
	var oy;

	oa = stride2offset( N, strideA );
	ob = stride2offset( N, strideB );
	oc = stride2offset( N, strideC );
	od = stride2offset( N, strideD );
	oin = stride2offset( N, strideIN );
	oy = stride2offset( N, strideY );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( job, N, a, strideA, oa, b, strideB, ob, c, strideC, oc, d, strideD, od, IN, strideIN, oin, y, strideY, oy, tol ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlagts;
