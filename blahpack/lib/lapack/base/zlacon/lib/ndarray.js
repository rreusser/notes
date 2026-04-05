
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Estimates the 1-norm of a square complex matrix A using reverse communication.
*
* This is a reverse communication interface routine. The caller must:
*
* 1.  Set `KASE[0]` = 0 on the first call.
*
* 2.  Call zlacon in a loop. After each return, if `KASE[0]` = 1,
*     compute `X = A*X` and call zlacon again; if `KASE[0]` = 2,
*     compute `X = A**H * X` and call zlacon again; if `KASE[0]` = 0,
*     the estimate is complete and `EST[0]` holds the result.
*
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} v - workspace vector of length N
* @param {integer} strideV - stride for V (in complex elements)
* @param {NonNegativeInteger} offsetV - starting index for V (in complex elements)
* @param {Complex128Array} x - input/output vector of length N
* @param {integer} strideX - stride for X (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for X (in complex elements)
* @param {Float64Array} est - in/out: est[0] is the estimated 1-norm
* @param {Int32Array} kase - in/out: kase[0] is the operation to perform
* @returns {void}
*/
function zlacon( N, v, strideV, offsetV, x, strideX, offsetX, est, kase ) { // eslint-disable-line max-len, max-params
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, v, strideV, offsetV, x, strideX, offsetX, est, kase ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlacon;
