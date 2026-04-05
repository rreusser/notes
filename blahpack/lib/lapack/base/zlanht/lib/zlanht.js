/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the norm of a complex Hermitian tridiagonal matrix A.
*
* @param {string} norm - `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Float64Array} d - diagonal elements (real), length N
* @param {integer} strideD - `d` stride length
* @param {Complex128Array} e - off-diagonal elements (complex), length N-1
* @param {integer} strideE - `e` stride length (in complex elements)
* @returns {number} the computed norm value
*/
function zlanht( norm, N, d, strideD, e, strideE ) {
	var od = stride2offset( N, strideD );
	var oe = stride2offset( N, strideE );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( norm !== 'max' && norm !== 'one-norm' && norm !== 'inf-norm' && norm !== 'frobenius' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid norm. Value: `%s`.', norm ) );
	}
	return base( norm, N, d, strideD, od, e, strideE, oe );
}


// EXPORTS //

module.exports = zlanht;
